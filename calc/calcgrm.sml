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

open Obj Parsing;
prim_val vector_ : int -> 'a -> 'a Vector.vector = 2 "make_vect";
prim_val update_ : 'a Vector.vector -> int -> 'a -> unit = 3 "set_vect_item";

open calcast
(* Line 7, file calcgrm.sml *)
val yytransl = #[
  257 (* DIV *),
  258 (* EOF *),
  259 (* MAS *),
  260 (* MENOS *),
  261 (* NRO *),
  262 (* PCOMA *),
  263 (* PDER *),
  264 (* PIZQ *),
  265 (* POR *),
    0];

val yylhs = "\255\255\
\\002\000\002\000\001\000\001\000\001\000\001\000\001\000\001\000\
\\001\000\000\000";

val yylen = "\002\000\
\\002\000\001\000\001\000\003\000\003\000\003\000\003\000\003\000\
\\002\000\002\000";

val yydefred = "\000\000\
\\000\000\000\000\002\000\000\000\003\000\000\000\000\000\010\000\
\\000\000\000\000\000\000\000\000\000\000\001\000\000\000\008\000\
\\000\000\000\000\000\000\000\000";

val yydgoto = "\002\000\
\\007\000\008\000";

val yysindex = "\002\000\
\\040\255\000\000\000\000\052\255\000\000\052\255\030\255\000\000\
\\000\000\034\255\052\255\052\255\052\255\000\000\052\255\000\000\
\\000\000\005\255\005\255\000\000";

val yyrindex = "\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\009\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\016\255\043\255\048\255\023\255";

val yygindex = "\000\000\
\\252\255\000\000";

val YYTABLESIZE = 60;
val yytable = "\009\000\
\\000\000\010\000\001\000\000\000\000\000\011\000\017\000\018\000\
\\019\000\009\000\020\000\009\000\009\000\015\000\009\000\009\000\
\\007\000\009\000\007\000\007\000\000\000\007\000\007\000\006\000\
\\007\000\006\000\006\000\000\000\006\000\006\000\011\000\006\000\
\\012\000\013\000\011\000\014\000\012\000\013\000\015\000\000\000\
\\016\000\003\000\015\000\004\000\005\000\004\000\004\000\006\000\
\\004\000\004\000\005\000\005\000\000\000\005\000\005\000\004\000\
\\005\000\000\000\000\000\006\000";

val yycheck = "\004\000\
\\255\255\006\000\001\000\255\255\255\255\001\001\011\000\012\000\
\\013\000\001\001\015\000\003\001\004\001\009\001\006\001\007\001\
\\001\001\009\001\003\001\004\001\255\255\006\001\007\001\001\001\
\\009\001\003\001\004\001\255\255\006\001\007\001\001\001\009\001\
\\003\001\004\001\001\001\006\001\003\001\004\001\009\001\255\255\
\\007\001\002\001\009\001\004\001\005\001\003\001\004\001\008\001\
\\006\001\007\001\003\001\004\001\255\255\006\001\007\001\004\001\
\\005\001\255\255\255\255\008\001";

val yyact = vector_ 11 (fn () => ((raise Fail "parser") : obj));
(* Rule 1, file calcgrm.y, line 14 *)
val _ = update_ yyact 1
(fn () => repr(let
val d__1__ = peekVal 1 : calcast.Expr
in
( Print (d__1__) ) end : calcast.Expr))
;
(* Rule 2, file calcgrm.y, line 15 *)
val _ = update_ yyact 2
(fn () => repr(let
in
( Nop ) end : calcast.Expr))
;
(* Rule 3, file calcgrm.y, line 17 *)
val _ = update_ yyact 3
(fn () => repr(let
val d__1__ = peekVal 0 : int
in
( Nro (d__1__) ) end : calcast.Expr))
;
(* Rule 4, file calcgrm.y, line 18 *)
val _ = update_ yyact 4
(fn () => repr(let
val d__1__ = peekVal 2 : calcast.Expr
val d__3__ = peekVal 0 : calcast.Expr
in
( Suma((d__1__),(d__3__)) ) end : calcast.Expr))
;
(* Rule 5, file calcgrm.y, line 19 *)
val _ = update_ yyact 5
(fn () => repr(let
val d__1__ = peekVal 2 : calcast.Expr
val d__3__ = peekVal 0 : calcast.Expr
in
( Resta((d__1__),(d__3__)) ) end : calcast.Expr))
;
(* Rule 6, file calcgrm.y, line 20 *)
val _ = update_ yyact 6
(fn () => repr(let
val d__1__ = peekVal 2 : calcast.Expr
val d__3__ = peekVal 0 : calcast.Expr
in
( Prod((d__1__),(d__3__)) ) end : calcast.Expr))
;
(* Rule 7, file calcgrm.y, line 21 *)
val _ = update_ yyact 7
(fn () => repr(let
val d__1__ = peekVal 2 : calcast.Expr
val d__3__ = peekVal 0 : calcast.Expr
in
( Div((d__1__),(d__3__)) ) end : calcast.Expr))
;
(* Rule 8, file calcgrm.y, line 22 *)
val _ = update_ yyact 8
(fn () => repr(let
val d__2__ = peekVal 1 : calcast.Expr
in
( (d__2__) ) end : calcast.Expr))
;
(* Rule 9, file calcgrm.y, line 23 *)
val _ = update_ yyact 9
(fn () => repr(let
val d__2__ = peekVal 0 : calcast.Expr
in
( Opuesto (d__2__) ) end : calcast.Expr))
;
(* Entry linea *)
val _ = update_ yyact 10 (fn () => raise yyexit (peekVal 0));
val yytables : parseTables =
  ( yyact,
    yytransl,
    yylhs,
    yylen,
    yydefred,
    yydgoto,
    yysindex,
    yyrindex,
    yygindex,
    YYTABLESIZE,
    yytable,
    yycheck );
fun linea lexer lexbuf = yyparse yytables 1 lexer lexbuf;
