type ty =
  | I1
  | I8
  | I16
  | I32
  | I64
  | Ptr of ty
  | Vector of int * ty
  | Struct of string
  | Array of int * ty
  | Void

type value =
  | Const of ty * int64
  | Const_vector of ty * int64 list
  | Null of ty
  | Undef of ty
  | Zero of ty
  | Local of int * ty
  | Param of string * ty
  | Global of string * ty

type binop =
  | Add
  | Sub
  | Mul
  | Sdiv
  | Srem
  | Udiv
  | Urem
  | And
  | Or
  | Xor
  | Shl
  | Lshr
  | Ashr

type cmp = Eq | Ne | Slt | Sle | Sgt | Sge | Ult | Ule | Ugt | Uge
type gep_index = Zero | Index of value
type extension = No_extension | Sign_extension | Zero_extension

type instr =
  | Bin of int * binop * ty * value * value
  | Cmp of int * cmp * ty * value * value
  | Alloca of int * ty * int
  | Load of int * ty * value * int
  | Store of ty * value * value * int
  | Gep of int * ty * value * gep_index list
  | Cast of int * string * ty * value * ty
  | Call of int option * extension * ty * string * (ty * extension * value) list
  | Phi of int * ty * (value * int) list
  | Select of int * value * value * value
  | Extract of int * ty * value * value
  | Insert of int * ty * value * value * value
  | Shuffle_zero of int * ty * value
  | String_ptr of int * int * int
  | Global_ptr of int * string * ty
  | Trap

type terminator =
  | Ret of (ty * value) option
  | Br of int
  | CondBr of value * int * int
  | Switch of ty * value * (int64 * int) list * int
  | Unreachable

type param = { name : string; ty : ty; extension : extension }
type linkage = Internal | External

type func = {
  name : string;
  params : param list;
  ret : ty;
  ret_extension : extension;
  blocks : block list;
  linkage : linkage;
  variadic : bool;
  asm_body : string option;
}

and block = { id : int; label : string; instrs : instr list; terminator : terminator }

type struct_def = { name : string; fields : ty list; tail_padding : int }

type global =
  | String_global of { name : string; bytes : string }
  | Array_global of { name : string; elem_ty : ty; elems : int64 list; align : int }

type module_ = {
  target_triple : string;
  data_layout : string;
  structs : struct_def list;
  globals : global list;
  funcs : func list;
  no_inline_function : string option;
}

let quote_identifier n =
  if
    String.for_all
      (function
        | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '.' | '$' -> true | _ -> false)
      n
  then n
  else "\"" ^ String.escaped n ^ "\""

let struct_name n = "%" ^ quote_identifier ("struct." ^ n)

let rec ty_name = function
  | I1 -> "i1"
  | I8 -> "i8"
  | I16 -> "i16"
  | I32 -> "i32"
  | I64 -> "i64"
  | Ptr _ -> "ptr"
  | Vector (n, t) -> Printf.sprintf "<%d x %s>" n (ty_name t)
  | Struct n -> struct_name n
  | Array (n, t) -> Printf.sprintf "[%d x %s]" n (ty_name t)
  | Void -> "void"

let value_ty = function
  | Const (t, _)
  | Const_vector (t, _)
  | Null t
  | Undef t
  | Zero t
  | Local (_, t)
  | Param (_, t)
  | Global (_, t) ->
      t

let ( let* ) result next =
  match result with Ok value -> next value | Error _ as e -> e

let rec type_equal left right =
  match (left, right) with
  | Ptr _, Ptr _ -> true
  | Vector (left_lanes, left_elem), Vector (right_lanes, right_elem) ->
      left_lanes = right_lanes && type_equal left_elem right_elem
  | Array (left_length, left_elem), Array (right_length, right_elem) ->
      left_length = right_length && type_equal left_elem right_elem
  | _ -> left = right

let integer_width = function
  | I1 -> Some 1
  | I8 -> Some 8
  | I16 -> Some 16
  | I32 -> Some 32
  | I64 -> Some 64
  | Ptr _ | Vector _ | Struct _ | Array _ | Void -> None

let integer_shape = function
  | Vector (lanes, elem) ->
      Option.map (fun width -> (Some lanes, width)) (integer_width elem)
  | ty -> Option.map (fun width -> (None, width)) (integer_width ty)

let is_integer ty = Option.is_some (integer_width ty)
let is_integer_like ty = Option.is_some (integer_shape ty)
let is_pointer = function Ptr _ -> true | _ -> false

let valid_extension ty = function
  | No_extension -> true
  | Sign_extension | Zero_extension -> is_integer ty

let is_equality_type = function
  | Vector (_, Ptr _) | Ptr _ -> true
  | ty -> is_integer_like ty

let rec valid_value_type = function
  | Void -> false
  | Vector (lanes, elem) -> lanes > 0 && (is_integer elem || is_pointer elem)
  | Array (length, elem) -> length >= 0 && valid_value_type elem
  | I1 | I8 | I16 | I32 | I64 | Ptr _ | Struct _ -> true

let integer_constant_fits ty value =
  match integer_width ty with
  | None -> false
  | Some 1 -> value = 0L || value = 1L
  | Some 64 -> true
  | Some width ->
      let minimum = Int64.neg (Int64.shift_left 1L (width - 1)) in
      let maximum = Int64.pred (Int64.shift_left 1L width) in
      value >= minimum && value <= maximum

let validate_value_form = function
  | Const (ty, value) ->
      if integer_constant_fits ty value then Ok ()
      else Error "integer constant does not fit its type"
  | Const_vector (Vector (lanes, elem), values) ->
      if lanes <= 0 then Error "vector constant has a non-positive lane count"
      else if List.length values <> lanes then
        Error "vector constant lane count does not match its type"
      else if List.for_all (integer_constant_fits elem) values then Ok ()
      else Error "vector constant element does not fit its type"
  | Const_vector _ -> Error "vector constant requires an integer vector type"
  | Null (Ptr _) -> Ok ()
  | Null _ -> Error "null constant requires a pointer type"
  | Undef ty | Zero ty | Local (_, ty) | Param (_, ty) | Global (_, ty) ->
      if valid_value_type ty then Ok () else Error "value has an invalid type"

let instruction_result = function
  | Bin (id, _, ty, _, _)
  | Load (id, ty, _, _)
  | Cast (id, _, _, _, ty)
  | Phi (id, ty, _)
  | Insert (id, ty, _, _, _)
  | Shuffle_zero (id, ty, _) ->
      Some (id, ty)
  | Alloca (id, ty, _) | Gep (id, ty, _, _) -> Some (id, Ptr ty)
  | Extract (id, Vector (_, elem), _, _) -> Some (id, elem)
  | Extract (id, ty, _, _) -> Some (id, ty)
  | Cmp (id, _, ty, _, _) ->
      Some (id, match ty with Vector (lanes, _) -> Vector (lanes, I1) | _ -> I1)
  | Select (id, _, yes, _) -> Some (id, value_ty yes)
  | Call (Some id, _, ty, _, _) -> Some (id, ty)
  | String_ptr (id, _, _) -> Some (id, Ptr I8)
  | Global_ptr (id, _, ty) -> Some (id, Ptr ty)
  | Store _ | Call (None, _, _, _, _) | Trap -> None

let terminator_successors = function
  | Ret _ | Unreachable -> []
  | Br successor -> [ successor ]
  | CondBr (_, yes, no) -> [ yes; no ]
  | Switch (_, _, cases, default) -> default :: List.map snd cases

let validate_function struct_names globals (func : func) =
  let fail format =
    Printf.ksprintf
      (fun message -> Error ("function `" ^ func.name ^ "` " ^ message))
      format
  in
  let rec references_defined_type = function
    | Struct name -> Hashtbl.mem struct_names name
    | Array (_, elem) | Vector (_, elem) -> references_defined_type elem
    | Ptr _ | I1 | I8 | I16 | I32 | I64 | Void -> true
  in
  let valid_module_value_type ty = valid_value_type ty && references_defined_type ty in
  let block_ids = Hashtbl.create (List.length func.blocks) in
  let blocks_by_id = Hashtbl.create (List.length func.blocks) in
  let predecessors = Hashtbl.create (List.length func.blocks) in
  let definitions = Hashtbl.create 64 in
  let definition_locations = Hashtbl.create 64 in
  let parameters = Hashtbl.create (List.length func.params) in
  let rec collect_blocks = function
    | [] -> Ok ()
    | (block : block) :: rest ->
        if block.id < 0 then fail "has negative block id %d" block.id
        else if Hashtbl.mem block_ids block.id then
          fail "has duplicate block id %d" block.id
        else (
          Hashtbl.add block_ids block.id ();
          Hashtbl.add blocks_by_id block.id block;
          Hashtbl.add predecessors block.id [];
          collect_blocks rest)
  in
  let rec collect_parameters = function
    | [] -> Ok ()
    | (parameter : param) :: rest ->
        if Hashtbl.mem parameters parameter.name then
          fail "has duplicate parameter name `%s`" parameter.name
        else if not (valid_module_value_type parameter.ty) then
          fail "parameter `%s` has an invalid type" parameter.name
        else if not (valid_extension parameter.ty parameter.extension) then
          fail "parameter `%s` has an invalid extension" parameter.name
        else (
          Hashtbl.add parameters parameter.name parameter.ty;
          collect_parameters rest)
  in
  let add_predecessor predecessor successor =
    match Hashtbl.find_opt predecessors successor with
    | None -> fail "block %d has unknown successor %d" predecessor successor
    | Some current ->
        if not (List.mem predecessor current) then
          Hashtbl.replace predecessors successor (predecessor :: current);
        Ok ()
  in
  let rec collect_successors predecessor = function
    | [] -> Ok ()
    | successor :: rest ->
        let* () = add_predecessor predecessor successor in
        collect_successors predecessor rest
  in
  let rec collect_edges = function
    | [] -> Ok ()
    | (block : block) :: rest ->
        let* () =
          collect_successors block.id (terminator_successors block.terminator)
        in
        collect_edges rest
  in
  let validate_entry_predecessors () =
    match func.blocks with
    | [] -> Ok ()
    | (entry : block) :: _ ->
        if Hashtbl.find predecessors entry.id = [] then Ok ()
        else fail "entry block %d has predecessors" entry.id
  in
  let add_definition block_id instruction_index instruction =
    match instruction_result instruction with
    | None -> Ok ()
    | Some (id, ty) ->
        if id < 0 then fail "block %d defines negative value id %d" block_id id
        else if Hashtbl.mem definitions id then fail "has duplicate value id %d" id
        else if not (valid_module_value_type ty) then
          fail "block %d value %d has invalid result type `%s`" block_id id (ty_name ty)
        else (
          Hashtbl.add definitions id ty;
          Hashtbl.add definition_locations id (block_id, instruction_index);
          Ok ())
  in
  let rec collect_instructions block_id instruction_index = function
    | [] -> Ok ()
    | instruction :: rest ->
        let* () = add_definition block_id instruction_index instruction in
        collect_instructions block_id (instruction_index + 1) rest
  in
  let rec collect_definitions = function
    | [] -> Ok ()
    | (block : block) :: rest ->
        let* () = collect_instructions block.id 0 block.instrs in
        collect_definitions rest
  in
  let validate_reference block_id value =
    let* () =
      if not (valid_module_value_type (value_ty value)) then
        fail "block %d has a value with an invalid type" block_id
      else
        match validate_value_form value with
        | Ok () -> Ok ()
        | Error message -> fail "block %d has invalid value: %s" block_id message
    in
    match value with
    | Local (id, claimed) -> (
        match Hashtbl.find_opt definitions id with
        | None -> fail "block %d uses undefined value %d" block_id id
        | Some actual when type_equal actual claimed -> Ok ()
        | Some _ -> fail "block %d value %d claims the wrong type" block_id id)
    | Param (name, claimed) -> (
        match Hashtbl.find_opt parameters name with
        | None -> fail "block %d uses unknown parameter `%s`" block_id name
        | Some actual when type_equal actual claimed -> Ok ()
        | Some _ -> fail "block %d parameter `%s` claims the wrong type" block_id name)
    | Global (name, claimed) -> (
        match Hashtbl.find_opt globals name with
        | None -> fail "block %d uses unknown global `%s`" block_id name
        | Some actual when type_equal claimed (Ptr actual) -> Ok ()
        | Some _ -> fail "block %d global `%s` claims the wrong type" block_id name)
    | Const _ | Const_vector _ | Null _ | Undef _ | Zero _ -> Ok ()
  in
  let operand block_id expected value =
    let* () = validate_reference block_id value in
    if type_equal expected (value_ty value) then Ok ()
    else fail "block %d has an operand with the wrong type" block_id
  in
  let pointer_operand block_id value =
    let* () = validate_reference block_id value in
    if is_pointer (value_ty value) then Ok ()
    else fail "block %d requires a pointer operand" block_id
  in
  let integer_operand block_id value =
    let* () = validate_reference block_id value in
    if is_integer (value_ty value) then Ok ()
    else fail "block %d requires an integer operand" block_id
  in
  let valid_alignment alignment = alignment > 0 && alignment land (alignment - 1) = 0 in
  let validate_alignment block_id alignment =
    if valid_alignment alignment then Ok ()
    else fail "block %d has invalid alignment %d" block_id alignment
  in
  let validate_cast block_id opcode source value destination =
    let* () = operand block_id source value in
    let integer_cast relation =
      match (integer_shape source, integer_shape destination) with
      | Some (source_lanes, source_width), Some (destination_lanes, destination_width)
        when source_lanes = destination_lanes && relation source_width destination_width
        ->
          Ok ()
      | _ -> fail "block %d has invalid `%s` types" block_id opcode
    in
    match opcode with
    | "zext" | "sext" -> integer_cast ( < )
    | "trunc" -> integer_cast ( > )
    | "ptrtoint" ->
        if is_pointer source && is_integer destination then Ok ()
        else fail "block %d has invalid `ptrtoint` types" block_id
    | "inttoptr" ->
        if is_integer source && is_pointer destination then Ok ()
        else fail "block %d has invalid `inttoptr` types" block_id
    | "bitcast" -> (
        match (integer_shape source, integer_shape destination) with
        | Some (source_lanes, source_width), Some (destination_lanes, destination_width)
          ->
            let source_count = Option.value ~default:1 source_lanes in
            let destination_count = Option.value ~default:1 destination_lanes in
            let rec gcd left right =
              if right = 0 then left else gcd right (left mod right)
            in
            let divisor = gcd source_width destination_width in
            let source_factor = destination_width / divisor in
            let destination_factor = source_width / divisor in
            if
              source_count mod source_factor = 0
              && destination_count mod destination_factor = 0
              && source_count / source_factor = destination_count / destination_factor
            then Ok ()
            else fail "block %d has unequal-width `bitcast` types" block_id
        | _ -> fail "block %d has invalid `bitcast` types" block_id)
    | _ -> fail "block %d has unknown cast opcode `%s`" block_id opcode
  in
  let rec validate_gep_indices block_id = function
    | [] -> Ok ()
    | Zero :: rest -> validate_gep_indices block_id rest
    | Index value :: rest ->
        let* () = integer_operand block_id value in
        validate_gep_indices block_id rest
  in
  let validate_phi block_id ty incoming =
    let rec validate_values seen = function
      | [] -> Ok seen
      | (value, predecessor) :: rest ->
          if List.mem predecessor seen then
            fail "block %d phi has duplicate incoming block %d" block_id predecessor
          else
            let* () = operand block_id ty value in
            validate_values (predecessor :: seen) rest
    in
    let* incoming_ids = validate_values [] incoming in
    let expected = Option.value ~default:[] (Hashtbl.find_opt predecessors block_id) in
    if incoming_ids = [] then fail "block %d has a phi without inputs" block_id
    else if List.sort compare incoming_ids = List.sort compare expected then Ok ()
    else fail "block %d phi inputs do not match its predecessors" block_id
  in
  let validate_instruction block_id = function
    | Bin (_, _, ty, left, right) ->
        if not (is_integer_like ty) then
          fail "block %d has a non-integer binary type" block_id
        else
          let* () = operand block_id ty left in
          operand block_id ty right
    | Cmp (_, comparison, ty, left, right) ->
        let valid_type =
          match comparison with
          | Eq | Ne -> is_equality_type ty
          | Slt | Sle | Sgt | Sge | Ult | Ule | Ugt | Uge -> is_integer_like ty
        in
        if not valid_type then fail "block %d has an invalid comparison type" block_id
        else
          let* () = operand block_id ty left in
          operand block_id ty right
    | Alloca (_, ty, alignment) ->
        if not (valid_module_value_type ty) then
          fail "block %d allocates an invalid type" block_id
        else validate_alignment block_id alignment
    | Load (_, ty, pointer, alignment) ->
        if not (valid_module_value_type ty) then
          fail "block %d loads an invalid type" block_id
        else
          let* () = pointer_operand block_id pointer in
          validate_alignment block_id alignment
    | Store (ty, value, pointer, alignment) ->
        if not (valid_module_value_type ty) then
          fail "block %d stores an invalid type" block_id
        else
          let* () = operand block_id ty value in
          let* () = pointer_operand block_id pointer in
          validate_alignment block_id alignment
    | Gep (_, ty, pointer, indices) ->
        if not (valid_module_value_type ty) then
          fail "block %d indexes an invalid type" block_id
        else
          let* () = pointer_operand block_id pointer in
          validate_gep_indices block_id indices
    | Cast (_, opcode, source, value, destination) ->
        validate_cast block_id opcode source value destination
    | Call (result, _, ty, _, arguments) ->
        let* () =
          match (result, ty) with
          | None, Void
          | Some _, (I1 | I8 | I16 | I32 | I64 | Ptr _ | Vector _ | Struct _ | Array _)
            ->
              Ok ()
          | None, _ -> fail "block %d discards a non-void call result" block_id
          | Some _, Void -> fail "block %d assigns a void call result" block_id
        in
        let rec validate_arguments = function
          | [] -> Ok ()
          | (argument_ty, _, value) :: rest ->
              let* () = operand block_id argument_ty value in
              validate_arguments rest
        in
        validate_arguments arguments
    | Phi (_, ty, incoming) -> validate_phi block_id ty incoming
    | Select (_, condition, yes, no) ->
        let selected = value_ty yes in
        let* () = validate_reference block_id condition in
        let valid_condition =
          match (value_ty condition, selected) with
          | I1, _ -> true
          | Vector (condition_lanes, I1), Vector (value_lanes, _) ->
              condition_lanes = value_lanes
          | _ -> false
        in
        if not valid_condition then
          fail "block %d has an invalid select condition" block_id
        else
          let* () = validate_reference block_id yes in
          operand block_id selected no
    | Extract (_, vector_ty, vector, index) -> (
        match vector_ty with
        | Vector _ ->
            let* () = operand block_id vector_ty vector in
            integer_operand block_id index
        | _ -> fail "block %d extracts from a non-vector type" block_id)
    | Insert (_, vector_ty, vector, index, value) -> (
        match vector_ty with
        | Vector (_, element_ty) ->
            let* () = operand block_id vector_ty vector in
            let* () = integer_operand block_id index in
            operand block_id element_ty value
        | _ -> fail "block %d inserts into a non-vector type" block_id)
    | Shuffle_zero (_, vector_ty, vector) -> (
        match vector_ty with
        | Vector _ -> operand block_id vector_ty vector
        | _ -> fail "block %d shuffles a non-vector type" block_id)
    | String_ptr (_, index, length) -> (
        let name = ".str." ^ string_of_int index in
        match Hashtbl.find_opt globals name with
        | Some (Array (actual_length, I8)) when actual_length = length -> Ok ()
        | Some (Array (_, I8)) ->
            fail "block %d string pointer `%s` has the wrong length" block_id name
        | Some _ ->
            fail "block %d string pointer `%s` has the wrong global type" block_id name
        | None -> fail "block %d references unknown string global `%s`" block_id name)
    | Global_ptr (_, name, ty) -> (
        match Hashtbl.find_opt globals name with
        | None -> fail "block %d references unknown global `%s`" block_id name
        | Some actual when type_equal actual ty -> Ok ()
        | Some _ -> fail "block %d global pointer `%s` has the wrong type" block_id name
        )
    | Trap -> Ok ()
  in
  let validate_phi_order block_id instructions =
    let rec validate seen_non_phi = function
      | [] -> Ok ()
      | Phi _ :: _ when seen_non_phi ->
          fail "block %d has a phi after a non-phi instruction" block_id
      | Phi _ :: rest -> validate false rest
      | _ :: rest -> validate true rest
    in
    validate false instructions
  in
  let rec validate_instructions block_id = function
    | [] -> Ok ()
    | instruction :: rest ->
        let* () = validate_instruction block_id instruction in
        validate_instructions block_id rest
  in
  let validate_terminator block_id = function
    | Ret None ->
        if func.ret = Void then Ok ()
        else fail "block %d returns void from a non-void function" block_id
    | Ret (Some (ty, value)) ->
        if func.ret = Void then
          fail "block %d returns a value from a void function" block_id
        else if not (type_equal func.ret ty) then
          fail "block %d returns the wrong type" block_id
        else operand block_id ty value
    | Br _ | Unreachable -> Ok ()
    | CondBr (condition, _, _) -> operand block_id I1 condition
    | Switch (ty, value, cases, _) ->
        if not (is_integer ty) then
          fail "block %d switches on a non-integer type" block_id
        else
          let* () = operand block_id ty value in
          let rec unique seen = function
            | [] -> Ok ()
            | (case, _) :: rest ->
                if List.mem case seen then
                  fail "block %d has duplicate switch case %Ld" block_id case
                else if integer_constant_fits ty case then unique (case :: seen) rest
                else fail "block %d has a switch case outside its type" block_id
          in
          unique [] cases
  in
  let rec validate_blocks = function
    | [] -> Ok ()
    | (block : block) :: rest ->
        let* () = validate_phi_order block.id block.instrs in
        let* () = validate_instructions block.id block.instrs in
        let* () = validate_terminator block.id block.terminator in
        validate_blocks rest
  in
  let reachable = Hashtbl.create (List.length func.blocks) in
  let rec mark_reachable block_id =
    if not (Hashtbl.mem reachable block_id) then (
      Hashtbl.add reachable block_id ();
      let block = Hashtbl.find blocks_by_id block_id in
      List.iter mark_reachable (terminator_successors block.terminator))
  in
  let dominators = Hashtbl.create (List.length func.blocks) in
  let compute_dominators () =
    match func.blocks with
    | [] -> ()
    | (entry : block) :: _ ->
        mark_reachable entry.id;
        let reachable_ids =
          List.filter_map
            (fun (block : block) ->
              if Hashtbl.mem reachable block.id then Some block.id else None)
            func.blocks
        in
        List.iter
          (fun block_id ->
            Hashtbl.add dominators block_id
              (if block_id = entry.id then [ entry.id ] else reachable_ids))
          reachable_ids;
        let intersection left right = List.filter (fun id -> List.mem id right) left in
        let changed = ref true in
        while !changed do
          changed := false;
          List.iter
            (fun block_id ->
              if block_id <> entry.id then
                let reachable_predecessors =
                  Option.value ~default:[] (Hashtbl.find_opt predecessors block_id)
                  |> List.filter (Hashtbl.mem reachable)
                in
                let common =
                  match reachable_predecessors with
                  | [] -> []
                  | first :: rest ->
                      List.fold_left
                        (fun current predecessor ->
                          intersection current (Hashtbl.find dominators predecessor))
                        (Hashtbl.find dominators first)
                        rest
                in
                let next = List.sort_uniq compare (block_id :: common) in
                if next <> Hashtbl.find dominators block_id then (
                  Hashtbl.replace dominators block_id next;
                  changed := true))
            reachable_ids
        done
  in
  let validate_dominance () =
    compute_dominators ();
    let validate_use report_block use_block use_index = function
      | Local (id, _) -> (
          match Hashtbl.find_opt definition_locations id with
          | None -> Ok ()
          | Some (definition_block, definition_index) ->
              let dominates =
                if not (Hashtbl.mem reachable use_block) then true
                else if definition_block = use_block then definition_index < use_index
                else
                  match Hashtbl.find_opt dominators use_block with
                  | Some blocks -> List.mem definition_block blocks
                  | None -> false
              in
              if dominates then Ok ()
              else fail "block %d value %d does not dominate its use" report_block id)
      | Const _ | Const_vector _ | Null _ | Undef _ | Zero _ | Param _ | Global _ ->
          Ok ()
    in
    let rec validate_uses report_block use_block use_index = function
      | [] -> Ok ()
      | value :: rest ->
          let* () = validate_use report_block use_block use_index value in
          validate_uses report_block use_block use_index rest
    in
    let instruction_uses = function
      | Bin (_, _, _, left, right) | Cmp (_, _, _, left, right) -> [ left; right ]
      | Load (_, _, pointer, _) -> [ pointer ]
      | Store (_, value, pointer, _) -> [ value; pointer ]
      | Gep (_, _, pointer, indices) ->
          pointer
          :: List.filter_map
               (function Zero -> None | Index value -> Some value)
               indices
      | Cast (_, _, _, value, _) -> [ value ]
      | Call (_, _, _, _, arguments) -> List.map (fun (_, _, value) -> value) arguments
      | Select (_, condition, yes, no) -> [ condition; yes; no ]
      | Extract (_, _, vector, index) -> [ vector; index ]
      | Insert (_, _, vector, index, value) -> [ vector; index; value ]
      | Shuffle_zero (_, _, vector) -> [ vector ]
      | Alloca _ | Phi _ | String_ptr _ | Global_ptr _ | Trap -> []
    in
    let rec validate_phi_uses block_id = function
      | [] -> Ok ()
      | (value, predecessor) :: rest ->
          let predecessor_block = Hashtbl.find blocks_by_id predecessor in
          let* () =
            validate_use block_id predecessor
              (List.length predecessor_block.instrs)
              value
          in
          validate_phi_uses block_id rest
    in
    let rec validate_instructions block_id instruction_index = function
      | [] -> Ok ()
      | Phi (_, _, incoming) :: rest ->
          let* () = validate_phi_uses block_id incoming in
          validate_instructions block_id (instruction_index + 1) rest
      | instruction :: rest ->
          let* () =
            validate_uses block_id block_id instruction_index
              (instruction_uses instruction)
          in
          validate_instructions block_id (instruction_index + 1) rest
    in
    let terminator_uses = function
      | Ret None | Br _ | Unreachable -> []
      | Ret (Some (_, value)) -> [ value ]
      | CondBr (condition, _, _) -> [ condition ]
      | Switch (_, value, _, _) -> [ value ]
    in
    let rec validate_block_uses = function
      | [] -> Ok ()
      | (block : block) :: rest ->
          let* () = validate_instructions block.id 0 block.instrs in
          let* () =
            validate_uses block.id block.id (List.length block.instrs)
              (terminator_uses block.terminator)
          in
          validate_block_uses rest
    in
    validate_block_uses func.blocks
  in
  let* () =
    if func.ret = Void || valid_module_value_type func.ret then Ok ()
    else fail "has an invalid return type"
  in
  let* () =
    if valid_extension func.ret func.ret_extension then Ok ()
    else fail "has an invalid return extension"
  in
  let* () = collect_blocks func.blocks in
  let* () = collect_parameters func.params in
  let* () = collect_edges func.blocks in
  let* () = validate_entry_predecessors () in
  let* () = collect_definitions func.blocks in
  let* () = validate_blocks func.blocks in
  validate_dominance ()

let validate module_ =
  let struct_names = Hashtbl.create (List.length module_.structs) in
  let rec collect_structs = function
    | [] -> Ok ()
    | (struct_def : struct_def) :: rest ->
        if Hashtbl.mem struct_names struct_def.name then
          Error ("module has duplicate struct name `" ^ struct_def.name ^ "`")
        else (
          Hashtbl.add struct_names struct_def.name struct_def;
          collect_structs rest)
  in
  let rec references_defined_type = function
    | Struct name -> Hashtbl.mem struct_names name
    | Array (_, elem) | Vector (_, elem) -> references_defined_type elem
    | Ptr _ | I1 | I8 | I16 | I32 | I64 | Void -> true
  in
  let validate_struct (struct_def : struct_def) =
    let rec validate_fields = function
      | [] -> Ok ()
      | field :: rest ->
          if not (valid_value_type field) then
            Error ("struct `" ^ struct_def.name ^ "` has an invalid field type")
          else if not (references_defined_type field) then
            Error ("struct `" ^ struct_def.name ^ "` references an unknown struct")
          else validate_fields rest
    in
    if struct_def.tail_padding < 0 then
      Error ("struct `" ^ struct_def.name ^ "` has negative tail padding")
    else validate_fields struct_def.fields
  in
  let rec validate_structs = function
    | [] -> Ok ()
    | struct_def :: rest ->
        let* () = validate_struct struct_def in
        validate_structs rest
  in
  let struct_states = Hashtbl.create (List.length module_.structs) in
  let rec validate_struct_cycles name =
    match Hashtbl.find_opt struct_states name with
    | Some 2 -> Ok ()
    | Some 1 -> Error ("struct `" ^ name ^ "` has a recursive value layout")
    | _ ->
        Hashtbl.replace struct_states name 1;
        let struct_def = Hashtbl.find struct_names name in
        let rec validate_type = function
          | Struct referenced -> validate_struct_cycles referenced
          | Array (_, elem) | Vector (_, elem) -> validate_type elem
          | Ptr _ | I1 | I8 | I16 | I32 | I64 | Void -> Ok ()
        in
        let rec validate_fields = function
          | [] -> Ok ()
          | field :: rest ->
              let* () = validate_type field in
              validate_fields rest
        in
        let* () = validate_fields struct_def.fields in
        Hashtbl.replace struct_states name 2;
        Ok ()
  in
  let rec validate_all_struct_cycles = function
    | [] -> Ok ()
    | (struct_def : struct_def) :: rest ->
        let* () = validate_struct_cycles struct_def.name in
        validate_all_struct_cycles rest
  in
  let globals = Hashtbl.create (List.length module_.globals) in
  let valid_alignment alignment = alignment > 0 && alignment land (alignment - 1) = 0 in
  let global_name = function
    | String_global { name; _ } | Array_global { name; _ } -> name
  in
  let global_type = function
    | String_global { bytes; _ } -> Array (String.length bytes, I8)
    | Array_global { elem_ty; elems; _ } -> Array (List.length elems, elem_ty)
  in
  let validate_global = function
    | String_global _ -> Ok ()
    | Array_global { name; elem_ty; elems; align } ->
        if not (is_integer elem_ty) then
          Error ("global `" ^ name ^ "` has a non-integer element type")
        else if not (List.for_all (integer_constant_fits elem_ty) elems) then
          Error ("global `" ^ name ^ "` has an element outside its type")
        else if not (valid_alignment align) then
          Error (Printf.sprintf "global `%s` has invalid alignment %d" name align)
        else Ok ()
  in
  let rec collect_globals = function
    | [] -> Ok ()
    | global :: rest ->
        let name = global_name global in
        if Hashtbl.mem globals name then
          Error ("module has duplicate global name `" ^ name ^ "`")
        else
          let* () = validate_global global in
          Hashtbl.add globals name (global_type global);
          collect_globals rest
  in
  let functions = Hashtbl.create (List.length module_.funcs) in
  let rec collect_functions = function
    | [] -> Ok ()
    | (func : func) :: rest ->
        if Hashtbl.mem globals func.name then
          Error ("module symbol `" ^ func.name ^ "` is both a global and a function")
        else if Hashtbl.mem functions func.name then
          Error ("module has duplicate function name `" ^ func.name ^ "`")
        else (
          Hashtbl.add functions func.name func;
          collect_functions rest)
  in
  let validate_call (caller : func) block_id result_extension result_ty target_name
      arguments =
    let fail format =
      Printf.ksprintf
        (fun message ->
          Error
            (Printf.sprintf "function `%s` block %d call to `%s` %s" caller.name
               block_id target_name message))
        format
    in
    match Hashtbl.find_opt functions target_name with
    | None -> fail "has no matching function"
    | Some target ->
        if not (type_equal result_ty target.ret) then fail "has the wrong return type"
        else if result_extension <> target.ret_extension then
          fail "has the wrong return extension"
        else if not (valid_extension result_ty result_extension) then
          fail "has an invalid return extension"
        else
          let fixed_count = List.length target.params in
          let argument_count = List.length arguments in
          if argument_count < fixed_count then
            fail "has %d arguments but requires at least %d" argument_count fixed_count
          else if (not target.variadic) && argument_count <> fixed_count then
            fail "has %d arguments but requires exactly %d" argument_count fixed_count
          else
            let rec validate_arguments index params args =
              match (params, args) with
              | [], rest ->
                  let rec validate_variadic index = function
                    | [] -> Ok ()
                    | (ty, extension, _) :: tail ->
                        if valid_extension ty extension then
                          validate_variadic (index + 1) tail
                        else fail "argument %d has an invalid extension" index
                  in
                  validate_variadic index rest
              | (parameter : param) :: params, (ty, extension, _) :: args ->
                  if not (type_equal ty parameter.ty) then
                    fail "argument %d has the wrong type" index
                  else if extension <> parameter.extension then
                    fail "argument %d has the wrong extension" index
                  else if not (valid_extension ty extension) then
                    fail "argument %d has an invalid extension" index
                  else validate_arguments (index + 1) params args
              | _ :: _, [] -> fail "is missing fixed argument %d" index
            in
            validate_arguments 0 target.params arguments
  in
  let validate_function_calls (func : func) =
    let rec validate_instructions block_id = function
      | [] -> Ok ()
      | Call (_, extension, ty, name, arguments) :: rest ->
          let* () = validate_call func block_id extension ty name arguments in
          validate_instructions block_id rest
      | _ :: rest -> validate_instructions block_id rest
    in
    let rec validate_blocks = function
      | [] -> Ok ()
      | (block : block) :: rest ->
          let* () = validate_instructions block.id block.instrs in
          validate_blocks rest
    in
    validate_blocks func.blocks
  in
  let rec validate_functions = function
    | [] -> Ok ()
    | func :: rest ->
        let* () = validate_function struct_names globals func in
        let* () = validate_function_calls func in
        validate_functions rest
  in
  let* () = collect_structs module_.structs in
  let* () = validate_structs module_.structs in
  let* () = validate_all_struct_cycles module_.structs in
  let* () = collect_globals module_.globals in
  let* () = collect_functions module_.funcs in
  let* () =
    match module_.no_inline_function with
    | None -> Ok ()
    | Some name -> (
        match Hashtbl.find_opt functions name with
        | Some func when func.blocks <> [] -> Ok ()
        | Some _ -> Error ("no-inline function `" ^ name ^ "` has no definition")
        | None -> Error ("no-inline function `" ^ name ^ "` does not exist"))
  in
  validate_functions module_.funcs

let symbol n =
  if
    String.for_all
      (function
        | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '.' | '$' -> true | _ -> false)
      n
  then "@" ^ n
  else "@\"" ^ String.escaped n ^ "\""

let extension_name = function
  | No_extension -> ""
  | Sign_extension -> "signext "
  | Zero_extension -> "zeroext "

let extension_attr = function
  | No_extension -> ""
  | Sign_extension -> " signext"
  | Zero_extension -> " zeroext"

let value_name = function
  | Const (I1, 0L) -> "false"
  | Const (I1, _) -> "true"
  | Const (_, v) -> Int64.to_string v
  | Const_vector (Vector (_, element_ty), values) ->
      "<"
      ^ String.concat ", "
          (List.map
             (fun value -> ty_name element_ty ^ " " ^ Int64.to_string value)
             values)
      ^ ">"
  | Const_vector (_, _) -> invalid_arg "vector constant requires a vector type"
  | Null _ -> "null"
  | Undef _ -> "poison"
  | Zero _ -> "zeroinitializer"
  | Local (i, _) -> Printf.sprintf "%%v%d" i
  | Param (n, _) -> "%" ^ n
  | Global (n, _) -> symbol n

let bin_name = function
  | Add -> "add"
  | Sub -> "sub"
  | Mul -> "mul"
  | Sdiv -> "sdiv"
  | Srem -> "srem"
  | Udiv -> "udiv"
  | Urem -> "urem"
  | And -> "and"
  | Or -> "or"
  | Xor -> "xor"
  | Shl -> "shl"
  | Lshr -> "lshr"
  | Ashr -> "ashr"

let cmp_name = function
  | Eq -> "eq"
  | Ne -> "ne"
  | Slt -> "slt"
  | Sle -> "sle"
  | Sgt -> "sgt"
  | Sge -> "sge"
  | Ult -> "ult"
  | Ule -> "ule"
  | Ugt -> "ugt"
  | Uge -> "uge"

let instr_line = function
  | Bin (i, op, t, a, b) ->
      Printf.sprintf "  %%v%d = %s %s %s, %s" i (bin_name op) (ty_name t) (value_name a)
        (value_name b)
  | Cmp (i, c, t, a, b) ->
      Printf.sprintf "  %%v%d = icmp %s %s %s, %s" i (cmp_name c) (ty_name t)
        (value_name a) (value_name b)
  | Alloca (i, t, a) -> Printf.sprintf "  %%v%d = alloca %s, align %d" i (ty_name t) a
  | Load (i, t, p, a) ->
      Printf.sprintf "  %%v%d = load %s, ptr %s, align %d" i (ty_name t) (value_name p)
        a
  | Store (t, v, p, a) ->
      Printf.sprintf "  store %s %s, ptr %s, align %d" (ty_name t) (value_name v)
        (value_name p) a
  | Gep (i, t, p, idxs) ->
      let one = function
        | Zero -> "i64 0"
        | Index v -> ty_name (value_ty v) ^ " " ^ value_name v
      in
      Printf.sprintf "  %%v%d = getelementptr %s, ptr %s, %s" i (ty_name t)
        (value_name p)
        (String.concat ", " (List.map one idxs))
  | Cast (i, k, st, v, dt) ->
      Printf.sprintf "  %%v%d = %s %s %s to %s" i k (ty_name st) (value_name v)
        (ty_name dt)
  | Call (Some i, extension, t, n, args) ->
      Printf.sprintf "  %%v%d = call %s%s %s(%s)" i (extension_name extension)
        (ty_name t) (symbol n)
        (String.concat ", "
           (List.map
              (fun (t, extension, v) ->
                ty_name t ^ " " ^ extension_name extension ^ value_name v)
              args))
  | Call (None, extension, t, n, args) ->
      Printf.sprintf "  call %s%s %s(%s)" (extension_name extension) (ty_name t)
        (symbol n)
        (String.concat ", "
           (List.map
              (fun (t, extension, v) ->
                ty_name t ^ " " ^ extension_name extension ^ value_name v)
              args))
  | Phi (i, t, xs) ->
      Printf.sprintf "  %%v%d = phi %s %s" i (ty_name t)
        (String.concat ", "
           (List.map (fun (v, b) -> Printf.sprintf "[ %s, %%b%d ]" (value_name v) b) xs))
  | Select (i, c, a, b) ->
      Printf.sprintf "  %%v%d = select %s %s, %s %s, %s %s" i
        (ty_name (value_ty c))
        (value_name c)
        (ty_name (value_ty a))
        (value_name a)
        (ty_name (value_ty b))
        (value_name b)
  | Extract (i, vt, v, l) ->
      Printf.sprintf "  %%v%d = extractelement %s %s, %s %s" i (ty_name vt)
        (value_name v)
        (ty_name (value_ty l))
        (value_name l)
  | Insert (i, vt, v, l, x) ->
      Printf.sprintf "  %%v%d = insertelement %s %s, %s %s, %s %s" i (ty_name vt)
        (value_name v)
        (ty_name (value_ty x))
        (value_name x)
        (ty_name (value_ty l))
        (value_name l)
  | Shuffle_zero (i, vt, v) ->
      let n = match vt with Vector (n, _) -> n | _ -> 0 in
      Printf.sprintf
        "  %%v%d = shufflevector %s %s, %s poison, <%d x i32> zeroinitializer" i
        (ty_name vt) (value_name v) (ty_name vt) n
  | String_ptr (i, index, n) ->
      Printf.sprintf "  %%v%d = getelementptr [%d x i8], ptr @.str.%d, i64 0, i64 0" i n
        index
  | Global_ptr (i, n, t) ->
      Printf.sprintf "  %%v%d = getelementptr %s, ptr @%s, i64 0" i (ty_name t) n
  | Trap -> "  call void @llvm.trap()"

let term_line = function
  | Ret None -> "  ret void"
  | Ret (Some (t, v)) -> Printf.sprintf "  ret %s %s" (ty_name t) (value_name v)
  | Br b -> Printf.sprintf "  br label %%b%d" b
  | CondBr (c, a, b) ->
      Printf.sprintf "  br i1 %s, label %%b%d, label %%b%d" (value_name c) a b
  | Switch (t, v, cases, d) ->
      Printf.sprintf "  switch %s %s, label %%b%d [ %s ]" (ty_name t) (value_name v) d
        (String.concat " "
           (List.map
              (fun (k, b) -> Printf.sprintf "%s %Ld, label %%b%d" (ty_name t) k b)
              cases))
  | Unreachable -> "  unreachable"

let param_string named p =
  ty_name p.ty ^ extension_attr p.extension ^ if named then " %" ^ p.name else ""

let render_bounded ~budget m =
  let buffer = Buffer.create 4096 in
  let exhausted = ref false in
  let add s =
    if not !exhausted then
      if Buffer.length buffer > budget - String.length s then exhausted := true
      else Buffer.add_string buffer s
  in
  let newline () = add "\n" in
  let add_escaped_bytes s =
    let len = String.length s in
    let plain j =
      let n = Char.code s.[j] in
      n >= 32 && n < 127 && s.[j] <> '"' && s.[j] <> '\\'
    in
    let rec run i =
      if i < len then
        if plain i then (
          let rec end_of_run j = if j < len && plain j then end_of_run (j + 1) else j in
          let stop = min (end_of_run i) (i + 256) in
          add (String.sub s i (stop - i));
          run stop)
        else (
          add (Printf.sprintf "\\%02X" (Char.code s.[i]));
          run (i + 1))
    in
    run 0
  in
  let add_params named variadic params =
    let emitted = ref false in
    List.iter
      (fun (p : param) ->
        if !emitted then add ", " else emitted := true;
        add (ty_name p.ty);
        add (extension_attr p.extension);
        if named then (
          add " %";
          add p.name))
      params;
    if variadic then if !emitted then add ", ..." else add "..."
  in
  add "; ModuleID = 'fas'";
  newline ();
  add "source_filename = \"fas\"";
  newline ();
  add "target datalayout = \"";
  add m.data_layout;
  add "\"";
  newline ();
  add "target triple = \"";
  add m.target_triple;
  add "\"";
  newline ();
  newline ();
  List.iter
    (fun s ->
      add (struct_name s.name);
      add " = type { ";
      List.iteri
        (fun i ty ->
          if i > 0 then add ", ";
          add (ty_name ty))
        s.fields;
      if s.tail_padding <> 0 then (
        if s.fields <> [] then add ", ";
        add "[";
        add (string_of_int s.tail_padding);
        add " x i8]");
      add " }";
      newline ())
    m.structs;
  List.iter
    (function
      | String_global { name; bytes } ->
          add "@";
          add name;
          add " = private unnamed_addr constant [";
          add (string_of_int (String.length bytes));
          add " x i8] c\"";
          add_escaped_bytes bytes;
          add "\"";
          newline ()
      | Array_global { name; elem_ty; elems; align } ->
          add "@";
          add name;
          add " = private unnamed_addr constant [";
          add (string_of_int (List.length elems));
          add " x ";
          add (ty_name elem_ty);
          add "] [";
          List.iteri
            (fun i v ->
              if i > 0 then add ", ";
              add (ty_name elem_ty);
              add " ";
              add (Int64.to_string v))
            elems;
          add "], align ";
          add (string_of_int align);
          newline ())
    m.globals;
  List.iter
    (fun f ->
      (if f.blocks = [] || Option.is_some f.asm_body then (
         add "declare ";
         add (extension_name f.ret_extension);
         add (ty_name f.ret);
         add " ";
         add (symbol f.name);
         add "(";
         add_params (f.blocks <> []) f.variadic f.params;
         add ")")
       else
         let link = if f.linkage = Internal then "internal " else "" in
         let no_inline =
           match m.no_inline_function with
           | Some name when name = f.name -> " noinline"
           | _ -> ""
         in
         add "define ";
         add link;
         add (extension_name f.ret_extension);
         add (ty_name f.ret);
         add " ";
         add (symbol f.name);
         add "(";
         add_params (f.blocks <> []) f.variadic f.params;
         add ")";
         add no_inline;
         add " {";
         newline ();
         List.iter
           (fun b ->
             add "b";
             add (string_of_int b.id);
             add ":";
             List.iter
               (fun instr ->
                 newline ();
                 add (instr_line instr))
               b.instrs;
             newline ();
             add (term_line b.terminator))
           f.blocks;
         newline ();
         add "}");
      newline ())
    m.funcs;
  if !exhausted then
    Error
      (Printf.sprintf "rendered LLVM text exceeds the configured limit of %d bytes"
         budget)
  else Ok (Buffer.contents buffer)

let render m =
  match render_bounded ~budget:max_int m with
  | Ok text -> text
  | Error _ -> assert false

let render_debug m =
  let global = function
    | String_global { name; bytes } ->
        Printf.sprintf "    String_global { name = %S; bytes = %S }" name bytes
    | Array_global { name; elem_ty; elems; align } ->
        Printf.sprintf
          "    Array_global { name = %S; elem_ty = %s; elems = [%s]; align = %d }" name
          (ty_name elem_ty)
          (String.concat "; " (List.map Int64.to_string elems))
          align
  in
  let block (b : block) =
    String.concat "\n"
      ([ Printf.sprintf "      Block { id = %d; label = %S; instrs = [" b.id b.label ]
      @ List.map
          (fun instruction -> "        " ^ String.trim (instr_line instruction))
          b.instrs
      @ [
          "      ];";
          "      terminator = " ^ String.trim (term_line b.terminator);
          "      }";
        ])
  in
  let func (f : func) =
    let params =
      f.params
      |> List.map (fun (p : param) -> Printf.sprintf "%s:%s" p.name (ty_name p.ty))
      |> String.concat ", "
    in
    String.concat "\n"
      ([
         Printf.sprintf
           "    Function { name = %S; params = [%s]; ret = %s; variadic = %b; blocks = \
            ["
           f.name params (ty_name f.ret) f.variadic;
       ]
      @ List.map block f.blocks @ [ "    ] }" ])
  in
  String.concat "\n"
    ([
       "Module {";
       Printf.sprintf "  target_triple = %S;" m.target_triple;
       Printf.sprintf "  data_layout = %S;" m.data_layout;
       Printf.sprintf "  no_inline_function = %s;"
         (match m.no_inline_function with
         | None -> "None"
         | Some name -> Printf.sprintf "Some %S" name);
       "  globals = [";
     ]
    @ List.map global m.globals @ [ "  ];"; "  functions = [" ] @ List.map func m.funcs
    @ [ "  ];"; "}" ])
  ^ "\n"

let raw_assembly m =
  m.funcs
  |> List.filter_map (fun (f : func) ->
      Option.map
        (fun raw ->
          Printf.sprintf
            "\n.text\n.globl %s\n.type %s,@function\n%s:\n%s\n.size %s, .-%s\n" f.name
            f.name f.name raw f.name f.name)
        f.asm_body)
  |> String.concat "\n"
