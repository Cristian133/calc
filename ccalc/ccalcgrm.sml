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

open Obj Parsing;
prim_val vector_ : int -> 'a -> 'a Vector.vector = 2 "make_vect";
prim_val update_ : 'a Vector.vector -> int -> 'a -> unit = 3 "set_vect_item";

open ccalcast
(* Line 7, file ccalcgrm.sml *)
val yytransl = #[
  257 (* ASIGN *),
  258 (* DIST *),
  259 (* DIV *),
  260 (* DO *),
  261 (* ELSE *),
  262 (* END *),
  263 (* EOF *),
  264 (* IF *),
  265 (* IGUAL *),
  266 (* MAS *),
  267 (* MAYIG *),
  268 (* MAYOR *),
  269 (* MENIG *),
  270 (* MENOR *),
  271 (* MENOS *),
  272 (* NRO *),
  273 (* PCOMA *),
  274 (* PDER *),
  275 (* PIZQ *),
  276 (* POR *),
  277 (* PRINT *),
  278 (* THEN *),
  279 (* VAR *),
  280 (* WHILE *),
    0];

val yylhs = "\255\255\
\\002\000\002\000\001\000\001\000\001\000\001\000\001\000\001\000\
\\001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
\\001\000\001\000\001\000\001\000\001\000\000\000";

val yylen = "\002\000\
\\002\000\001\000\001\000\001\000\003\000\003\000\003\000\003\000\
\\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\\002\000\006\000\005\000\002\000\003\000\002\000";

val yydefred = "\000\000\
\\000\000\000\000\002\000\000\000\000\000\003\000\000\000\000\000\
\\000\000\000\000\000\000\022\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\001\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\015\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\019\000\000\000";

val yydgoto = "\002\000\
\\011\000\012\000";

val yysindex = "\001\000\
\\121\000\000\000\000\000\133\000\133\000\000\000\133\000\133\000\
\\004\255\133\000\170\255\000\000\154\255\000\000\186\255\253\255\
\\133\000\205\255\133\000\133\000\000\000\133\000\133\000\133\000\
\\133\000\133\000\133\000\133\000\133\000\133\000\133\000\000\000\
\\253\255\133\000\016\000\000\000\016\000\008\255\016\000\016\000\
\\016\000\016\000\008\255\253\255\000\000\221\255\237\255\133\000\
\\000\000\253\255";

val yyrindex = "\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\036\255\000\000\000\000\000\000\000\000\057\255\000\000\254\255\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\017\000\000\000\023\000\078\255\042\000\118\255\048\000\067\000\
\\073\000\092\000\137\255\098\000\099\255\000\000\000\000\000\000\
\\000\000\117\000";

val yygindex = "\000\000\
\\252\255\000\000";

val YYTABLESIZE = 413;
val yytable = "\013\000\
\\014\000\001\000\015\000\016\000\017\000\018\000\000\000\000\000\
\\000\000\000\000\020\000\000\000\033\000\000\000\035\000\036\000\
\\000\000\037\000\038\000\039\000\040\000\041\000\042\000\043\000\
\\044\000\045\000\046\000\030\000\000\000\047\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\004\000\004\000\004\000\
\\004\000\004\000\004\000\050\000\004\000\004\000\004\000\004\000\
\\004\000\004\000\004\000\000\000\004\000\004\000\000\000\004\000\
\\000\000\004\000\017\000\017\000\017\000\017\000\017\000\017\000\
\\000\000\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
\\000\000\017\000\017\000\000\000\017\000\000\000\017\000\008\000\
\\008\000\008\000\008\000\008\000\008\000\000\000\008\000\008\000\
\\008\000\008\000\008\000\008\000\008\000\000\000\008\000\008\000\
\\000\000\008\000\000\000\008\000\007\000\007\000\007\000\007\000\
\\007\000\007\000\000\000\007\000\007\000\007\000\007\000\007\000\
\\007\000\007\000\000\000\007\000\007\000\000\000\007\000\005\000\
\\007\000\005\000\005\000\005\000\005\000\000\000\005\000\005\000\
\\005\000\005\000\005\000\005\000\005\000\000\000\005\000\005\000\
\\000\000\000\000\006\000\005\000\006\000\006\000\006\000\006\000\
\\000\000\006\000\006\000\006\000\006\000\006\000\006\000\006\000\
\\000\000\006\000\006\000\019\000\020\000\000\000\006\000\000\000\
\\000\000\000\000\022\000\023\000\024\000\025\000\026\000\027\000\
\\028\000\000\000\029\000\019\000\020\000\030\000\000\000\031\000\
\\021\000\000\000\022\000\023\000\024\000\025\000\026\000\027\000\
\\028\000\000\000\029\000\019\000\020\000\030\000\000\000\000\000\
\\000\000\000\000\022\000\023\000\024\000\025\000\026\000\027\000\
\\028\000\000\000\029\000\032\000\000\000\030\000\019\000\020\000\
\\034\000\000\000\000\000\000\000\000\000\022\000\023\000\024\000\
\\025\000\026\000\027\000\028\000\000\000\029\000\019\000\020\000\
\\030\000\048\000\000\000\000\000\000\000\022\000\023\000\024\000\
\\025\000\026\000\027\000\028\000\000\000\029\000\019\000\020\000\
\\030\000\000\000\049\000\000\000\000\000\022\000\023\000\024\000\
\\025\000\026\000\027\000\028\000\000\000\029\000\019\000\020\000\
\\030\000\020\000\020\000\020\000\020\000\022\000\023\000\024\000\
\\025\000\026\000\027\000\028\000\000\000\000\000\020\000\020\000\
\\030\000\000\000\020\000\020\000\016\000\016\000\016\000\016\000\
\\000\000\023\000\014\000\014\000\014\000\014\000\028\000\000\000\
\\000\000\016\000\016\000\030\000\000\000\000\000\016\000\014\000\
\\014\000\000\000\000\000\000\000\014\000\013\000\013\000\013\000\
\\013\000\000\000\000\000\012\000\012\000\012\000\012\000\000\000\
\\000\000\000\000\013\000\013\000\000\000\000\000\000\000\013\000\
\\012\000\012\000\000\000\000\000\000\000\012\000\011\000\011\000\
\\011\000\011\000\000\000\000\000\010\000\010\000\010\000\010\000\
\\000\000\000\000\000\000\011\000\011\000\000\000\000\000\000\000\
\\011\000\010\000\010\000\000\000\000\000\000\000\010\000\009\000\
\\009\000\009\000\009\000\000\000\000\000\021\000\021\000\021\000\
\\021\000\000\000\000\000\000\000\009\000\009\000\000\000\000\000\
\\000\000\009\000\021\000\021\000\000\000\000\000\000\000\021\000\
\\018\000\018\000\018\000\018\000\000\000\000\000\000\000\003\000\
\\004\000\000\000\000\000\000\000\000\000\018\000\018\000\005\000\
\\006\000\000\000\018\000\007\000\004\000\008\000\000\000\009\000\
\\010\000\000\000\000\000\005\000\006\000\000\000\000\000\007\000\
\\000\000\008\000\000\000\009\000\010\000";

val yycheck = "\004\000\
\\005\000\001\000\007\000\008\000\001\001\010\000\255\255\255\255\
\\255\255\255\255\003\001\255\255\017\000\255\255\019\000\020\000\
\\255\255\022\000\023\000\024\000\025\000\026\000\027\000\028\000\
\\029\000\030\000\031\000\020\001\255\255\034\000\255\255\255\255\
\\255\255\255\255\255\255\255\255\255\255\002\001\003\001\004\001\
\\005\001\006\001\007\001\048\000\009\001\010\001\011\001\012\001\
\\013\001\014\001\015\001\255\255\017\001\018\001\255\255\020\001\
\\255\255\022\001\002\001\003\001\004\001\005\001\006\001\007\001\
\\255\255\009\001\010\001\011\001\012\001\013\001\014\001\015\001\
\\255\255\017\001\018\001\255\255\020\001\255\255\022\001\002\001\
\\003\001\004\001\005\001\006\001\007\001\255\255\009\001\010\001\
\\011\001\012\001\013\001\014\001\015\001\255\255\017\001\018\001\
\\255\255\020\001\255\255\022\001\002\001\003\001\004\001\005\001\
\\006\001\007\001\255\255\009\001\010\001\011\001\012\001\013\001\
\\014\001\015\001\255\255\017\001\018\001\255\255\020\001\002\001\
\\022\001\004\001\005\001\006\001\007\001\255\255\009\001\010\001\
\\011\001\012\001\013\001\014\001\015\001\255\255\017\001\018\001\
\\255\255\255\255\002\001\022\001\004\001\005\001\006\001\007\001\
\\255\255\009\001\010\001\011\001\012\001\013\001\014\001\015\001\
\\255\255\017\001\018\001\002\001\003\001\255\255\022\001\255\255\
\\255\255\255\255\009\001\010\001\011\001\012\001\013\001\014\001\
\\015\001\255\255\017\001\002\001\003\001\020\001\255\255\022\001\
\\007\001\255\255\009\001\010\001\011\001\012\001\013\001\014\001\
\\015\001\255\255\017\001\002\001\003\001\020\001\255\255\255\255\
\\255\255\255\255\009\001\010\001\011\001\012\001\013\001\014\001\
\\015\001\255\255\017\001\018\001\255\255\020\001\002\001\003\001\
\\004\001\255\255\255\255\255\255\255\255\009\001\010\001\011\001\
\\012\001\013\001\014\001\015\001\255\255\017\001\002\001\003\001\
\\020\001\005\001\255\255\255\255\255\255\009\001\010\001\011\001\
\\012\001\013\001\014\001\015\001\255\255\017\001\002\001\003\001\
\\020\001\255\255\006\001\255\255\255\255\009\001\010\001\011\001\
\\012\001\013\001\014\001\015\001\255\255\017\001\002\001\003\001\
\\020\001\004\001\005\001\006\001\007\001\009\001\010\001\011\001\
\\012\001\013\001\014\001\015\001\255\255\255\255\017\001\018\001\
\\020\001\255\255\003\001\022\001\004\001\005\001\006\001\007\001\
\\255\255\010\001\004\001\005\001\006\001\007\001\015\001\255\255\
\\255\255\017\001\018\001\020\001\255\255\255\255\022\001\017\001\
\\018\001\255\255\255\255\255\255\022\001\004\001\005\001\006\001\
\\007\001\255\255\255\255\004\001\005\001\006\001\007\001\255\255\
\\255\255\255\255\017\001\018\001\255\255\255\255\255\255\022\001\
\\017\001\018\001\255\255\255\255\255\255\022\001\004\001\005\001\
\\006\001\007\001\255\255\255\255\004\001\005\001\006\001\007\001\
\\255\255\255\255\255\255\017\001\018\001\255\255\255\255\255\255\
\\022\001\017\001\018\001\255\255\255\255\255\255\022\001\004\001\
\\005\001\006\001\007\001\255\255\255\255\004\001\005\001\006\001\
\\007\001\255\255\255\255\255\255\017\001\018\001\255\255\255\255\
\\255\255\022\001\017\001\018\001\255\255\255\255\255\255\022\001\
\\004\001\005\001\006\001\007\001\255\255\255\255\255\255\007\001\
\\008\001\255\255\255\255\255\255\255\255\017\001\018\001\015\001\
\\016\001\255\255\022\001\019\001\008\001\021\001\255\255\023\001\
\\024\001\255\255\255\255\015\001\016\001\255\255\255\255\019\001\
\\255\255\021\001\255\255\023\001\024\001";

val yyact = vector_ 23 (fn () => ((raise Fail "parser") : obj));
(* Rule 1, file ccalcgrm.y, line 23 *)
val _ = update_ yyact 1
(fn () => repr(let
val d__1__ = peekVal 1 : ccalcast.Expr
in
( (d__1__) ) end : ccalcast.Expr))
;
(* Rule 2, file ccalcgrm.y, line 24 *)
val _ = update_ yyact 2
(fn () => repr(let
in
( Nop ) end : ccalcast.Expr))
;
(* Rule 3, file ccalcgrm.y, line 26 *)
val _ = update_ yyact 3
(fn () => repr(let
val d__1__ = peekVal 0 : int
in
( Nro (d__1__) ) end : ccalcast.Expr))
;
(* Rule 4, file ccalcgrm.y, line 27 *)
val _ = update_ yyact 4
(fn () => repr(let
val d__1__ = peekVal 0 : string
in
( Var (d__1__) ) end : ccalcast.Expr))
;
(* Rule 5, file ccalcgrm.y, line 28 *)
val _ = update_ yyact 5
(fn () => repr(let
val d__1__ = peekVal 2 : ccalcast.Expr
val d__3__ = peekVal 0 : ccalcast.Expr
in
( Suma((d__1__),(d__3__)) ) end : ccalcast.Expr))
;
(* Rule 6, file ccalcgrm.y, line 29 *)
val _ = update_ yyact 6
(fn () => repr(let
val d__1__ = peekVal 2 : ccalcast.Expr
val d__3__ = peekVal 0 : ccalcast.Expr
in
( Resta((d__1__),(d__3__)) ) end : ccalcast.Expr))
;
(* Rule 7, file ccalcgrm.y, line 30 *)
val _ = update_ yyact 7
(fn () => repr(let
val d__1__ = peekVal 2 : ccalcast.Expr
val d__3__ = peekVal 0 : ccalcast.Expr
in
( Prod((d__1__),(d__3__)) ) end : ccalcast.Expr))
;
(* Rule 8, file ccalcgrm.y, line 31 *)
val _ = update_ yyact 8
(fn () => repr(let
val d__1__ = peekVal 2 : ccalcast.Expr
val d__3__ = peekVal 0 : ccalcast.Expr
in
( Div((d__1__),(d__3__)) ) end : ccalcast.Expr))
;
(* Rule 9, file ccalcgrm.y, line 32 *)
val _ = update_ yyact 9
(fn () => repr(let
val d__1__ = peekVal 2 : ccalcast.Expr
val d__3__ = peekVal 0 : ccalcast.Expr
in
( Cmp(Menor, (d__1__),(d__3__)) ) end : ccalcast.Expr))
;
(* Rule 10, file ccalcgrm.y, line 33 *)
val _ = update_ yyact 10
(fn () => repr(let
val d__1__ = peekVal 2 : ccalcast.Expr
val d__3__ = peekVal 0 : ccalcast.Expr
in
( Cmp(MenIg, (d__1__),(d__3__)) ) end : ccalcast.Expr))
;
(* Rule 11, file ccalcgrm.y, line 34 *)
val _ = update_ yyact 11
(fn () => repr(let
val d__1__ = peekVal 2 : ccalcast.Expr
val d__3__ = peekVal 0 : ccalcast.Expr
in
( Cmp(Mayor, (d__1__),(d__3__)) ) end : ccalcast.Expr))
;
(* Rule 12, file ccalcgrm.y, line 35 *)
val _ = update_ yyact 12
(fn () => repr(let
val d__1__ = peekVal 2 : ccalcast.Expr
val d__3__ = peekVal 0 : ccalcast.Expr
in
( Cmp(MayIg, (d__1__),(d__3__)) ) end : ccalcast.Expr))
;
(* Rule 13, file ccalcgrm.y, line 36 *)
val _ = update_ yyact 13
(fn () => repr(let
val d__1__ = peekVal 2 : ccalcast.Expr
val d__3__ = peekVal 0 : ccalcast.Expr
in
( Cmp(Igual, (d__1__),(d__3__)) ) end : ccalcast.Expr))
;
(* Rule 14, file ccalcgrm.y, line 37 *)
val _ = update_ yyact 14
(fn () => repr(let
val d__1__ = peekVal 2 : ccalcast.Expr
val d__3__ = peekVal 0 : ccalcast.Expr
in
( Cmp(Dist, (d__1__),(d__3__)) ) end : ccalcast.Expr))
;
(* Rule 15, file ccalcgrm.y, line 38 *)
val _ = update_ yyact 15
(fn () => repr(let
val d__2__ = peekVal 1 : ccalcast.Expr
in
( (d__2__) ) end : ccalcast.Expr))
;
(* Rule 16, file ccalcgrm.y, line 39 *)
val _ = update_ yyact 16
(fn () => repr(let
val d__1__ = peekVal 2 : string
val d__3__ = peekVal 0 : ccalcast.Expr
in
( Asign((d__1__), (d__3__)) ) end : ccalcast.Expr))
;
(* Rule 17, file ccalcgrm.y, line 40 *)
val _ = update_ yyact 17
(fn () => repr(let
val d__2__ = peekVal 0 : ccalcast.Expr
in
( Opuesto (d__2__) ) end : ccalcast.Expr))
;
(* Rule 18, file ccalcgrm.y, line 41 *)
val _ = update_ yyact 18
(fn () => repr(let
val d__2__ = peekVal 4 : ccalcast.Expr
val d__4__ = peekVal 2 : ccalcast.Expr
val d__6__ = peekVal 0 : ccalcast.Expr
in
( If((d__2__), (d__4__), (d__6__)) ) end : ccalcast.Expr))
;
(* Rule 19, file ccalcgrm.y, line 42 *)
val _ = update_ yyact 19
(fn () => repr(let
val d__2__ = peekVal 3 : ccalcast.Expr
val d__4__ = peekVal 1 : ccalcast.Expr
in
( While((d__2__), (d__4__)) ) end : ccalcast.Expr))
;
(* Rule 20, file ccalcgrm.y, line 43 *)
val _ = update_ yyact 20
(fn () => repr(let
val d__2__ = peekVal 0 : ccalcast.Expr
in
( Print (d__2__) ) end : ccalcast.Expr))
;
(* Rule 21, file ccalcgrm.y, line 44 *)
val _ = update_ yyact 21
(fn () => repr(let
val d__1__ = peekVal 2 : ccalcast.Expr
val d__3__ = peekVal 0 : ccalcast.Expr
in
( Sec((d__1__), (d__3__)) ) end : ccalcast.Expr))
;
(* Entry linea *)
val _ = update_ yyact 22 (fn () => raise yyexit (peekVal 0));
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