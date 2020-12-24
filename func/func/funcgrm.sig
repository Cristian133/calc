local
in
datatype token =
    COMA
  | DOSP
  | END
  | EOF
  | FUN
  | ID of string
  | MAS
  | NRO of int
  | PD
  | PI
  | POR
end;

val prog :
  (Lexing.lexbuf -> token) -> Lexing.lexbuf -> funcast.Decl list * funcast.Expr;
