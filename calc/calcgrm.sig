local
in
datatype token =
    DIV
  | EOF
  | MAS
  | MENOS
  | NRO of int
  | PCOMA
  | PDER
  | PIZQ
  | POR
end;

val linea :
  (Lexing.lexbuf -> token) -> Lexing.lexbuf -> calcast.Expr;
