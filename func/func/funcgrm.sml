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

open Obj Parsing;
prim_val vector_ : int -> 'a -> 'a Vector.vector = 2 "make_vect";
prim_val update_ : 'a Vector.vector -> int -> 'a -> unit = 3 "set_vect_item";

open funcast
(* Line 7, file funcgrm.sml *)
val yytransl = #[
  257 (* COMA *),
  258 (* DOSP *),
  259 (* END *),
  260 (* EOF *),
  261 (* FUN *),
  262 (* ID *),
  263 (* MAS *),
  264 (* NRO *),
  265 (* PD *),
  266 (* PI *),
  267 (* POR *),
    0];

val yylhs = "\255\255\
\\001\000\002\000\002\000\003\000\005\000\005\000\006\000\006\000\
\\004\000\004\000\004\000\004\000\004\000\004\000\007\000\007\000\
\\008\000\008\000\000\000";

val yylen = "\002\000\
\\003\000\002\000\000\000\008\000\002\000\000\000\003\000\000\000\
\\001\000\001\000\003\000\003\000\004\000\003\000\002\000\000\000\
\\003\000\000\000\002\000";

val yydefred = "\000\000\
\\000\000\000\000\000\000\019\000\000\000\000\000\000\000\000\000\
\\009\000\000\000\000\000\002\000\000\000\000\000\000\000\001\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\014\000\000\000\
\\000\000\000\000\005\000\000\000\000\000\015\000\013\000\000\000\
\\000\000\000\000\007\000\000\000\017\000\004\000";

val yydgoto = "\002\000\
\\004\000\005\000\006\000\011\000\020\000\027\000\022\000\030\000";

val yysindex = "\001\000\
\\252\254\000\000\004\255\000\000\037\255\252\254\006\255\007\255\
\\000\000\037\255\031\255\000\000\018\255\037\255\039\255\000\000\
\\037\255\037\255\024\255\021\255\020\255\027\255\000\000\028\255\
\\000\000\034\255\000\000\042\255\037\255\000\000\000\000\024\255\
\\037\255\020\255\000\000\030\255\000\000\000\000";

val yyrindex = "\000\000\
\\043\255\000\000\000\000\000\000\000\000\043\255\000\000\002\255\
\\000\000\000\000\000\000\000\000\045\255\046\255\000\000\000\000\
\\000\000\000\000\047\255\000\000\048\255\000\000\000\000\025\255\
\\011\255\000\000\000\000\000\000\000\000\000\000\000\000\047\255\
\\000\000\048\255\000\000\000\000\000\000\000\000";

val yygindex = "\000\000\
\\000\000\046\000\000\000\246\255\000\000\026\000\000\000\025\000";

val YYTABLESIZE = 59;
val yytable = "\015\000\
\\003\000\001\000\010\000\021\000\010\000\010\000\024\000\025\000\
\\010\000\007\000\010\000\012\000\010\000\012\000\012\000\013\000\
\\014\000\012\000\034\000\012\000\029\000\012\000\036\000\019\000\
\\026\000\011\000\017\000\011\000\011\000\028\000\018\000\011\000\
\\038\000\011\000\016\000\031\000\017\000\017\000\018\000\032\000\
\\018\000\018\000\008\000\033\000\009\000\017\000\010\000\023\000\
\\003\000\018\000\003\000\012\000\003\000\006\000\016\000\008\000\
\\018\000\035\000\037\000";

val yycheck = "\010\000\
\\005\001\001\000\001\001\014\000\003\001\004\001\017\000\018\000\
\\007\001\006\001\009\001\001\001\011\001\003\001\004\001\010\001\
\\010\001\007\001\029\000\009\001\001\001\011\001\033\000\006\001\
\\001\001\001\001\007\001\003\001\004\001\009\001\011\001\007\001\
\\003\001\009\001\004\001\009\001\007\001\007\001\011\001\006\001\
\\011\001\011\001\006\001\002\001\008\001\007\001\010\001\009\001\
\\006\001\011\001\008\001\006\000\010\001\009\001\009\001\009\001\
\\009\001\032\000\034\000";

val yyact = vector_ 20 (fn () => ((raise Fail "parser") : obj));
(* Rule 1, file funcgrm.y, line 21 *)
val _ = update_ yyact 1
(fn () => repr(let
val d__1__ = peekVal 2 : funcast.Decl list
val d__2__ = peekVal 1 : funcast.Expr
in
( ((d__1__), (d__2__)) ) end : funcast.Decl list * funcast.Expr))
;
(* Rule 2, file funcgrm.y, line 23 *)
val _ = update_ yyact 2
(fn () => repr(let
val d__1__ = peekVal 1 : funcast.Decl
val d__2__ = peekVal 0 : funcast.Decl list
in
( (d__1__)::(d__2__) ) end : funcast.Decl list))
;
(* Rule 3, file funcgrm.y, line 24 *)
val _ = update_ yyact 3
(fn () => repr(let
in
( [] ) end : funcast.Decl list))
;
(* Rule 4, file funcgrm.y, line 27 *)
val _ = update_ yyact 4
(fn () => repr(let
val d__2__ = peekVal 6 : string
val d__4__ = peekVal 4 : string list
val d__7__ = peekVal 1 : funcast.Expr
in
( Func{func=(d__2__), args=(d__4__), body=(d__7__)} ) end : funcast.Decl))
;
(* Rule 5, file funcgrm.y, line 29 *)
val _ = update_ yyact 5
(fn () => repr(let
val d__1__ = peekVal 1 : string
val d__2__ = peekVal 0 : string list
in
( (d__1__)::(d__2__) ) end : string list))
;
(* Rule 6, file funcgrm.y, line 30 *)
val _ = update_ yyact 6
(fn () => repr(let
in
( [] ) end : string list))
;
(* Rule 7, file funcgrm.y, line 32 *)
val _ = update_ yyact 7
(fn () => repr(let
val d__2__ = peekVal 1 : string
val d__3__ = peekVal 0 : string list
in
( (d__2__)::(d__3__) ) end : string list))
;
(* Rule 8, file funcgrm.y, line 33 *)
val _ = update_ yyact 8
(fn () => repr(let
in
( [] ) end : string list))
;
(* Rule 9, file funcgrm.y, line 35 *)
val _ = update_ yyact 9
(fn () => repr(let
val d__1__ = peekVal 0 : int
in
( Nro (d__1__) ) end : funcast.Expr))
;
(* Rule 10, file funcgrm.y, line 36 *)
val _ = update_ yyact 10
(fn () => repr(let
val d__1__ = peekVal 0 : string
in
( Id (d__1__) ) end : funcast.Expr))
;
(* Rule 11, file funcgrm.y, line 37 *)
val _ = update_ yyact 11
(fn () => repr(let
val d__1__ = peekVal 2 : funcast.Expr
val d__3__ = peekVal 0 : funcast.Expr
in
( Suma((d__1__), (d__3__)) ) end : funcast.Expr))
;
(* Rule 12, file funcgrm.y, line 38 *)
val _ = update_ yyact 12
(fn () => repr(let
val d__1__ = peekVal 2 : funcast.Expr
val d__3__ = peekVal 0 : funcast.Expr
in
( Prod((d__1__), (d__3__)) ) end : funcast.Expr))
;
(* Rule 13, file funcgrm.y, line 40 *)
val _ = update_ yyact 13
(fn () => repr(let
val d__1__ = peekVal 3 : string
val d__3__ = peekVal 1 : funcast.Expr list
in
( Call((d__1__), (d__3__)) ) end : funcast.Expr))
;
(* Rule 14, file funcgrm.y, line 41 *)
val _ = update_ yyact 14
(fn () => repr(let
val d__2__ = peekVal 1 : funcast.Expr
in
( (d__2__) ) end : funcast.Expr))
;
(* Rule 15, file funcgrm.y, line 43 *)
val _ = update_ yyact 15
(fn () => repr(let
val d__1__ = peekVal 1 : funcast.Expr
val d__2__ = peekVal 0 : funcast.Expr list
in
( (d__1__)::(d__2__) ) end : funcast.Expr list))
;
(* Rule 16, file funcgrm.y, line 44 *)
val _ = update_ yyact 16
(fn () => repr(let
in
( [] ) end : funcast.Expr list))
;
(* Rule 17, file funcgrm.y, line 47 *)
val _ = update_ yyact 17
(fn () => repr(let
val d__2__ = peekVal 1 : funcast.Expr
val d__3__ = peekVal 0 : funcast.Expr list
in
( (d__2__)::(d__3__) ) end : funcast.Expr list))
;
(* Rule 18, file funcgrm.y, line 48 *)
val _ = update_ yyact 18
(fn () => repr(let
in
( [] ) end : funcast.Expr list))
;
(* Entry prog *)
val _ = update_ yyact 19 (fn () => raise yyexit (peekVal 0));
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
fun prog lexer lexbuf = yyparse yytables 1 lexer lexbuf;
