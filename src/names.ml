type parser_operation =
  | Zext
  | Sext
  | Trunc
  | Bitcast
  | Sizeof
  | Alignof
  | Offsetof
  | Splat
  | Legacy_ptr_add
  | Legacy_ptr_add_bytes

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

type type_constructor = Legacy_ptr | Array | Vector
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

let parser_type_name name =
  List.mem name scalar_type_names || List.mem name [ "ptr"; "arr"; "vec" ]

let type_constructor = function
  | "ptr" -> Some Legacy_ptr
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
    ("volatile_load", Reserved);
    ("volatile_store", Reserved);
    ("addr_bits", Reserved);
    ("addr_from_bits", Reserved);
    ("handle_addr", Reserved);
    ("handle_from_addr", Reserved);
  ]

let operation_names = List.map fst operations

let reserved_binding_name name =
  List.mem name primitive_type_names
  || List.mem name literal_names
  || Option.is_some (List.assoc_opt name operations)

let parser_operation name =
  match List.assoc_opt name operations with
  | Some (Parser operation) -> Some operation
  | Some (Value _ | Reserved) -> None
  | None -> (
      match name with
      | "ptr_add" -> Some Legacy_ptr_add
      | "ptr_add_bytes" -> Some Legacy_ptr_add_bytes
      | _ -> None)

let value_operation name =
  match List.assoc_opt name operations with
  | Some (Value operation) -> Some operation
  | Some (Parser _ | Reserved) -> None
  | None -> None
