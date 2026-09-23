open Sema_constants
open Sema_context
module String_set = Set.Make (String)

let error span message = Error [ Diag.error span message ]
let ( let* ) r f = match r with Error e -> Error e | Ok x -> f x

let collect ~source_obj ~structs ~named_types ~scalar_consts (program : Ast.program) =
  let consts = ref (List.rev scalar_consts) and arrays = ref [] in
  let consts_names =
    ref (String_set.of_list (List.map (fun (n, _, _) -> n) scalar_consts))
  and arrays_names = ref String_set.empty in
  let eval_const_item = function
    | Ast.Const { name; ty; value; span } -> (
        if String_set.mem name !consts_names then Ok ()
        else if String_set.mem name !arrays_names then
          error span (Printf.sprintf "duplicate const `%s`" name)
        else
          let* t = source_obj span ty in
          match (t, value) with
          | Hir.Array (n, elem), Ast.Array_lit (xs, _) ->
              if List.length xs <> n then error span "const array length mismatch"
              else
                let rec values acc = function
                  | [] -> Ok (List.rev acc)
                  | x :: rest ->
                      let* vt, v =
                        const_expr ~structs ~named_types ~arrays:!arrays !consts
                          (Some elem) x
                      in
                      if equal vt elem then values (v :: acc) rest
                      else error (Ast.expr_span x) "const array element type mismatch"
                in
                let* vs = values [] xs in
                arrays := (name, t, vs) :: !arrays;
                arrays_names := String_set.add name !arrays_names;
                Ok ()
          | Hir.Array _, _ -> error span "const array needs a brace-list initializer"
          | (Hir.Vec _ as vector_ty), _ ->
              let* actual_ty, values =
                vector_const_expr ~structs ~named_types ~arrays:!arrays !consts
                  (Some vector_ty) value
              in
              if equal actual_ty vector_ty then (
                arrays := (name, vector_ty, values) :: !arrays;
                arrays_names := String_set.add name !arrays_names;
                Ok ())
              else error span "constant initializer type mismatch"
          | _, Ast.Array_lit _ -> error span "brace-list requires an array type"
          | _, _ ->
              let* vt, v =
                const_expr ~structs ~named_types ~arrays:!arrays !consts (Some t) value
              in
              if equal vt t then (
                consts := (name, t, v) :: !consts;
                consts_names := String_set.add name !consts_names;
                Ok ())
              else error span "constant initializer type mismatch")
    | _ -> Ok ()
  in
  let* () = Result_list.iter eval_const_item program.items in
  let consts_ordered = List.rev !consts and arrays_ordered = List.rev !arrays in
  Ok (consts_ordered, arrays_ordered, List.map (fun (name, _, _) -> name) arrays_ordered)
