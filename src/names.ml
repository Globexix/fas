type parser_operation =
  | Zext
  | Sext
  | Trunc
  | Bitcast
  | Sizeof
  | Alignof
  | Offsetof
  | Splat

type value_operation =
  | Len
  | Rotl
  | Rotr
  | Popcount
  | Ctz
  | Clz
  | Add_sat
  | Sub_sat
  | Mul_hi
  | Any
  | All
  | Select
  | Shuffle
  | Permute
  | Reduce_sum
  | Reduce_min
  | Reduce_max
  | Reduce_and
  | Reduce_or
  | Reduce_xor
  | Compress
  | Expand
  | Addr_bits
  | Addr_from_bits
  | Handle_addr
  | Handle_from_addr

type type_constructor = Array | Vector | Address | Handle
type literal = True | False | Null

let scalar_type_names =
  [
    "bool";
    "void";
    "u8";
    "u16";
    "u32";
    "u64";
    "i8";
    "i16";
    "i32";
    "i64";
    "usize";
    "isize";
  ]

let primitive_type_names = scalar_type_names @ [ "addr"; "handle"; "arr"; "vec" ]
let literal_names = [ "true"; "false"; "null" ]
let reserved_float_names = [ "f32"; "f64"; "sqrt"; "fma"; "floor"; "ceil"; "round" ]
let reserved_float_name name = List.mem name reserved_float_names
let reserved_v05_float_names = [ "f32"; "f64"; "sqrt"; "fma"; "floor"; "ceil"; "round" ]
let reserved_float_type_names = [ "f32"; "f64" ]

let reserved_float_message name =
  if List.mem name reserved_v05_float_names then
    Printf.sprintf "`%s` is reserved for v0.5 floating point" name
  else Printf.sprintf "`%s` is reserved" name

let reserved_binding_message name =
  if List.mem name reserved_v05_float_names then
    Printf.sprintf
      "`%s` is reserved for v0.5 floating point and cannot be used as a name" name
  else Printf.sprintf "`%s` is reserved and cannot be used as a name" name

let parser_type_name name =
  List.mem name scalar_type_names
  || List.mem name reserved_float_type_names
  || List.mem name [ "ptr"; "addr"; "handle"; "arr"; "vec" ]

let type_constructor = function
  | "addr" -> Some Address
  | "handle" -> Some Handle
  | "arr" -> Some Array
  | "vec" -> Some Vector
  | _ -> None

let literal = function
  | "true" -> Some True
  | "false" -> Some False
  | "null" -> Some Null
  | _ -> None

type operation_kind = Parser of parser_operation | Value of value_operation | Reserved

let operations =
  [
    ("zext", Parser Zext);
    ("sext", Parser Sext);
    ("trunc", Parser Trunc);
    ("bitcast", Parser Bitcast);
    ("sizeof", Parser Sizeof);
    ("alignof", Parser Alignof);
    ("offsetof", Parser Offsetof);
    ("len", Value Len);
    ("splat", Parser Splat);
    ("rotl", Value Rotl);
    ("rotr", Value Rotr);
    ("popcount", Value Popcount);
    ("clz", Value Clz);
    ("ctz", Value Ctz);
    ("select", Value Select);
    ("any", Value Any);
    ("all", Value All);
    ("add_sat", Value Add_sat);
    ("sub_sat", Value Sub_sat);
    ("mul_hi", Value Mul_hi);
    ("reduce_sum", Value Reduce_sum);
    ("reduce_min", Value Reduce_min);
    ("reduce_max", Value Reduce_max);
    ("reduce_and", Value Reduce_and);
    ("reduce_or", Value Reduce_or);
    ("reduce_xor", Value Reduce_xor);
    ("shuffle", Value Shuffle);
    ("permute", Value Permute);
    ("compress", Value Compress);
    ("expand", Value Expand);
    ("masked_load", Reserved);
    ("masked_store", Reserved);
    ("gather", Reserved);
    ("gather_bytes", Reserved);
    ("scatter", Reserved);
    ("scatter_bytes", Reserved);
    ("copy", Reserved);
    ("call_addr", Reserved);
    ("volatile_load", Reserved);
    ("volatile_store", Reserved);
    ("addr_bits", Value Addr_bits);
    ("addr_from_bits", Value Addr_from_bits);
    ("handle_addr", Value Handle_addr);
    ("handle_from_addr", Value Handle_from_addr);
    ("sqrt", Reserved);
    ("fma", Reserved);
    ("floor", Reserved);
    ("ceil", Reserved);
    ("round", Reserved);
  ]

let operation_names = List.map fst operations

let reserved_binding_name name =
  List.mem name primitive_type_names
  || List.mem name literal_names
  || List.mem name [ "view"; "use"; "var" ]
  || reserved_float_name name
  || Option.is_some (List.assoc_opt name operations)

let parser_operation name =
  match List.assoc_opt name operations with
  | Some (Parser operation) -> Some operation
  | Some (Value _ | Reserved) -> None
  | None -> None

let value_operation name =
  match List.assoc_opt name operations with
  | Some (Value operation) -> Some operation
  | Some (Parser _ | Reserved) -> None
  | None -> None
