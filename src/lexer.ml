let is_space = function ' ' | '\t' | '\r' -> true | _ -> false
let is_digit c = c >= '0' && c <= '9'
let is_hex c = is_digit c || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')

let is_ident_start c =
  (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_'

let is_ident_continue c = is_ident_start c || is_digit c

let keyword = function
  | "fn" -> Token.Kw_fn
  | "return" -> Kw_return
  | "if" -> Kw_if
  | "else" -> Kw_else
  | "while" -> Kw_while
  | "break" -> Kw_break
  | "continue" -> Kw_continue
  | "const" -> Kw_const
  | "struct" -> Kw_struct
  | "opaque" -> Kw_opaque
  | "extern" -> Kw_extern
  | "defer" -> Kw_defer
  | "asm" -> Kw_asm
  | "for" -> Kw_for
  | "switch" -> Kw_switch
  | "case" -> Kw_case
  | "default" -> Kw_default
  | name -> Ident name

let lex ?(limits = Limits.default) source =
  let text = Source.text source in
  let n = String.length text in
  let diagnostic offset message =
    Diag.error
      (Source.span source ~start_offset:offset ~end_offset:(min n (offset + 1)))
      message
  in
  let rec loop offset count tokens =
