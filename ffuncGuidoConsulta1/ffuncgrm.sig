local
in
datatype token =
    ARROBA
  | ASIGN
  | COMA
  | CR
  | DIST
  | DIV
  | DOSP
  | ELSE
  | END
  | EOF
  | FLECHA
  | FUN
  | ID of string
  | IF
  | IGUAL
  | IN
  | LET
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
  | PUNTO
  | READ
  | TEXTO of string
  | THEN
  | TIPO of ffunctipo.Tipo
  | UNIT
end;

val prog :
  (Lexing.lexbuf -> token) -> Lexing.lexbuf -> ffuncast.Expr;
