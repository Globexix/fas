type value =
  | Obj of (string * value) list
  | Arr of value list
  | Str of string
  | Num of string
  | Bool of bool
  | Null

type input = {
  channel : in_channel;
  data : bytes;
  mutable pos : int;
  mutable size : int;
}

let refill i =
  if i.pos = i.size then (
    i.size <- input i.channel i.data 0 (Bytes.length i.data);
    i.pos <- 0;
    if i.size = 0 then raise End_of_file)

let peek i =
  refill i;
  Bytes.get i.data i.pos

let take i =
  let c = peek i in
  i.pos <- i.pos + 1;
  c

let rec space i =
  match peek i with
  | ' ' | '\n' | '\r' | '\t' ->
      ignore (take i);
      space i
  | _ -> ()

let expect i c =
  space i;
  if take i <> c then failwith "invalid Clang JSON"

let skip_string i =
  let escaped = ref false and closed = ref false in
  while not !closed do
    refill i;
    let c = Bytes.get i.data i.pos in
    i.pos <- i.pos + 1;
    if !escaped then escaped := false
    else if c = '\\' then escaped := true
    else if c = '"' then closed := true
  done

let skip_value i =
  space i;
  match take i with
  | '"' -> skip_string i
  | '{' | '[' ->
      let depth = ref 1 and quoted = ref false and escaped = ref false in
      while !depth > 0 do
        refill i;
        while i.pos < i.size && !depth > 0 do
          let c = Bytes.get i.data i.pos in
          i.pos <- i.pos + 1;
          if !quoted then (
            if !escaped then escaped := false
            else if c = '\\' then escaped := true
            else if c = '"' then quoted := false)
          else
            match c with
            | '"' -> quoted := true
            | '{' | '[' -> incr depth
            | '}' | ']' -> decr depth
            | _ -> ()
        done
      done
  | _ ->
      while
        match peek i with
        | ',' | ']' | '}' | ' ' | '\n' | '\r' | '\t' -> false
        | _ -> true
      do
        ignore (take i)
      done

let hex i =
  let digit = function
    | '0' .. '9' as c -> Char.code c - 48
    | 'a' .. 'f' as c -> Char.code c - 87
    | 'A' .. 'F' as c -> Char.code c - 55
    | _ -> failwith "invalid Clang JSON escape"
  in
  let n = ref 0 in
  for _ = 1 to 4 do
    n := (!n lsl 4) lor digit (take i)
  done;
  !n

let read_string i =
  let b = Buffer.create 32 in
  let rec loop () =
    match take i with
    | '"' -> Buffer.contents b
    | '\\' ->
        (match take i with
        | '"' -> Buffer.add_char b '"'
        | '\\' -> Buffer.add_char b '\\'
        | '/' -> Buffer.add_char b '/'
        | 'b' -> Buffer.add_char b '\b'
        | 'f' -> Buffer.add_char b '\012'
        | 'n' -> Buffer.add_char b '\n'
        | 'r' -> Buffer.add_char b '\r'
        | 't' -> Buffer.add_char b '\t'
        | 'u' ->
            let first = hex i in
            let code =
              if first >= 0xd800 && first <= 0xdbff then (
                if take i <> '\\' || take i <> 'u' then
                  failwith "invalid Clang JSON surrogate pair";
                let second = hex i in
                if second < 0xdc00 || second > 0xdfff then
                  failwith "invalid Clang JSON surrogate pair";
                0x10000 + (((first - 0xd800) lsl 10) lor (second - 0xdc00)))
              else first
            in
            Buffer.add_utf_8_uchar b (Uchar.of_int code)
        | _ -> failwith "invalid Clang JSON escape");
        loop ()
    | c ->
        Buffer.add_char b c;
        loop ()
  in
  loop ()

let supported = function
  | "FunctionDecl" | "VarDecl" | "TypedefDecl" | "EnumDecl" | "RecordDecl" -> true
  | _ -> false

let string = function Str s -> Some s | _ -> None

let keep_field = function
  | "kind" | "id" | "decl" | "name" | "type" | "loc" | "value" | "storageClass"
  | "inline" | "tagUsed" | "fixedUnderlyingType" | "isBitfield" | "isImplicit" | "inner"
  | "qualType" | "desugaredQualType" | "file" | "line" ->
      true
  | _ -> false

let rec json i =
  space i;
  match peek i with
  | '"' ->
      ignore (take i);
      Str (read_string i)
  | '{' -> object_value i
  | '[' -> array_value i
  | 't' -> literal i "true" (Bool true)
  | 'f' -> literal i "false" (Bool false)
  | 'n' -> literal i "null" Null
  | _ ->
      let b = Buffer.create 12 in
      while
        match peek i with
        | ',' | ']' | '}' | ' ' | '\n' | '\r' | '\t' -> false
        | _ -> true
      do
        Buffer.add_char b (take i)
      done;
      Num (Buffer.contents b)

and literal i text value =
  String.iter (fun c -> if take i <> c then failwith "invalid Clang JSON") text;
  value

and array_value i =
  expect i '[';
  space i;
  if peek i = ']' then (
    ignore (take i);
    Arr [])
  else
    let rec loop acc =
      let v = json i in
      match
        space i;
        take i
      with
      | ']' -> Arr (List.rev (v :: acc))
      | ',' -> loop (v :: acc)
      | _ -> failwith "invalid Clang JSON array"
    in
    loop []

and object_value i =
  expect i '{';
  space i;
  if peek i = '}' then (
    ignore (take i);
    Obj [])
  else
    let rec loop kind acc =
      expect i '"';
      let key = read_string i in
      expect i ':';
      let keep = keep_field key in
      let skip_inner =
        key = "inner"
        && (kind = "VarDecl" || kind = "RecordDecl" || kind = "FieldDecl"
           || String.ends_with ~suffix:"Stmt" kind)
      in
      let value =
        if (not keep) || skip_inner then (
          skip_value i;
          None)
        else Some (json i)
      in
      let kind =
        if key = "kind" then Option.value ~default:kind (Option.bind value string)
        else kind
      in
      match
        space i;
        take i
      with
      | '}' ->
          Obj
            (match value with
            | None -> List.rev acc
            | Some value -> List.rev ((key, value) :: acc))
      | ',' ->
          loop kind (match value with None -> acc | Some value -> (key, value) :: acc)
      | _ -> failwith "invalid Clang JSON object"
    in
    loop "" []

let declaration i =
  expect i '{';
  expect i '"';
  let first_field = read_string i in
  expect i ':';
  let first_value =
    if first_field = "id" then Some (json i)
    else (
      skip_value i;
      None)
  in
  expect i ',';
  expect i '"';
  if read_string i <> "kind" then failwith "Clang declaration kind order changed";
  expect i ':';
  expect i '"';
  let kind = read_string i in
  let keep = supported kind in
  let acc =
    if keep then
      Option.fold ~none:[] ~some:(fun value -> [ (first_field, value) ]) first_value
    else []
  in
  let rec rest acc =
    match
      space i;
      take i
    with
    | '}' -> if keep then Some (Obj (List.rev (("kind", Str kind) :: acc))) else None
    | ',' ->
        expect i '"';
        let key = read_string i in
        expect i ':';
        if keep && keep_field key then rest ((key, json i) :: acc)
        else (
          skip_value i;
          rest acc)
    | _ -> failwith "invalid Clang declaration"
  in
  rest acc

let declarations channel =
  let i = { channel; data = Bytes.create 65536; pos = 0; size = 0 } in
  expect i '{';
  let rec root acc =
    space i;
    if peek i = '}' then (
      ignore (take i);
      List.rev acc)
    else (
      expect i '"';
      let key = read_string i in
      expect i ':';
      let acc =
        if key <> "inner" then (
          skip_value i;
          acc)
        else (
          expect i '[';
          space i;
          if peek i = ']' then (
            ignore (take i);
            acc)
          else
            let rec items acc =
              let item = declaration i in
              let acc = Option.fold ~none:acc ~some:(fun v -> v :: acc) item in
              match
                space i;
                take i
              with
              | ']' -> acc
              | ',' -> items acc
              | _ -> failwith "invalid Clang declaration array"
            in
            items acc)
      in
      match
        space i;
        take i
      with
      | '}' -> List.rev acc
      | ',' -> root acc
      | _ -> failwith "invalid Clang translation unit")
  in
  root []

let field name = function Obj fields -> List.assoc_opt name fields | _ -> None
let array = function Arr values -> values | _ -> []
