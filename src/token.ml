type kind =
  | Ident of string
  | Int of string
  | String of string
  | CString of string
  | Kw_fn
  | Kw_return
  | Kw_if
  | Kw_else
  | Kw_while
  | Kw_break
  | Kw_continue
  | Kw_const
  | Kw_struct
  | Kw_opaque
  | Kw_extern
  | Kw_defer
  | Kw_asm
  | Kw_for
  | Kw_switch
  | Kw_case
  | Kw_default
  | Lparen
  | Rparen
  | Lbrace
  | Rbrace
  | Lbracket
  | Rbracket
  | Comma
  | Dot
  | Ellipsis
  | Semi
  | At
  | Question
  | Colon
  | Assign
  | Plus
  | Minus
  | Star
  | Slash
  | Percent
  | Plus_eq
  | Minus_eq
  | Star_eq
  | Slash_eq
  | Percent_eq
  | Amp_eq
  | Pipe_eq
  | Caret_eq
  | Amp
  | Pipe
  | Caret
  | Tilde
  | Eqeq
  | Neq
  | Lt
  | Le
  | Gt
  | Ge
  | Andand
  | Oror
  | Not
  | Newline
  | Eof

type t = { kind : kind; span : Span.t }

let show = function
  | Ident s -> "`" ^ s ^ "`"
  | Int s -> "`" ^ s ^ "`"
  | String s -> "\"" ^ s ^ "\""
  | CString s -> "c\"" ^ s ^ "\""
  | Kw_fn -> "`fn`"
  | Kw_return -> "`return`"
  | Kw_if -> "`if`"
  | Kw_else -> "`else`"
  | Kw_while -> "`while`"
  | Kw_break -> "`break`"
  | Kw_continue -> "`continue`"
  | Kw_const -> "`const`"
  | Kw_struct -> "`struct`"
  | Kw_opaque -> "`opaque`"
  | Kw_extern -> "`extern`"
  | Kw_defer -> "`defer`"
  | Kw_asm -> "`asm`"
  | Kw_for -> "`for`"
  | Kw_switch -> "`switch`"
  | Kw_case -> "`case`"
  | Kw_default -> "`default`"
  | Lparen -> "`(`"
  | Rparen -> "`)`"
  | Lbrace -> "`{`"
  | Rbrace -> "`}`"
  | Lbracket -> "`[`"
  | Rbracket -> "`]`"
  | Comma -> "`,`"
  | Dot -> "`.`"
  | Ellipsis -> "`...`"
  | Semi -> "`;`"
  | At -> "`@`"
  | Question -> "`?`"
  | Colon -> "`:`"
  | Assign -> "`=`"
  | Plus -> "`+`"
  | Minus -> "`-`"
  | Star -> "`*`"
  | Slash -> "`/`"
  | Percent -> "`%`"
  | Plus_eq -> "`+=`"
  | Minus_eq -> "`-=`"
  | Star_eq -> "`*=`"
  | Slash_eq -> "`/=`"
  | Percent_eq -> "`%=`"
  | Amp_eq -> "`&=`"
  | Pipe_eq -> "`|=`"
  | Caret_eq -> "`^=`"
  | Amp -> "`&`"
  | Pipe -> "`|`"
  | Caret -> "`^`"
  | Tilde -> "`~`"
  | Eqeq -> "`==`"
  | Neq -> "`!=`"
  | Lt -> "`<`"
  | Le -> "`<=`"
  | Gt -> "`>`"
  | Ge -> "`>=`"
  | Andand -> "`&&`"
  | Oror -> "`||`"
  | Not -> "`!`"
  | Newline -> "newline"
  | Eof -> "end of file"
