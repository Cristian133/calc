local
in
datatype token =
    ASIGN
  | DIST
  | DIV
  | DO
  | ELSE
  | END
  | EOF
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
  | PDER
  | PIZQ
  | POR
  | PRINT
  | THEN
  | VAR of string
  | WHILE
end;

val linea :
  (Lexing.lexbuf -> token) -> Lexing.lexbuf -> ccalcast.Expr;
