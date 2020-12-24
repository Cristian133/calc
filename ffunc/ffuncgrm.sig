local
in
datatype token =
    ARRO
  | ASIGN
  | COMA
  | DIST
  | DIV
  | DOSP
  | ELSE
  | END
  | EOF
  | FLECHA
  | FN
  | FUN
  | ID of string
  | IF
  | IGUAL
  | MAS
  | MAYIG
  | MAYOR
  | MENIG
  | MENOR
  | MENOS
  | NRO of int
  | PCOMA
  | PD
  | PI
  | POR
  | PRINT
  | THEN
end;

val prog :
  (Lexing.lexbuf -> token) -> Lexing.lexbuf -> ffuncast.Decl list * ffuncast.Expr;
