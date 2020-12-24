local
in
datatype token =
    ASIGN
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

open Obj Parsing;
prim_val vector_ : int -> 'a -> 'a Vector.vector = 2 "make_vect";
prim_val update_ : 'a Vector.vector -> int -> 'a -> unit = 3 "set_vect_item";

open ffuncast
(* Line 7, file ffuncgrm.sml *)
val yytransl = #[
  257 (* ASIGN *),
  258 (* COMA *),
  259 (* CR *),
  260 (* DIST *),
  261 (* DIV *),
  262 (* DOSP *),
  263 (* ELSE *),
  264 (* END *),
  265 (* EOF *),
  266 (* FLECHA *),
  267 (* FUN *),
  268 (* ID *),
  269 (* IF *),
  270 (* IGUAL *),
  271 (* IN *),
  272 (* LET *),
  273 (* MAS *),
  274 (* MAYIG *),
  275 (* MAYOR *),
  276 (* MENIG *),
  277 (* MENOR *),
  278 (* MENOS *),
  279 (* NRO *),
  280 (* PCOMA *),
  281 (* PD *),
  282 (* PI *),
  283 (* POR *),
  284 (* PRINT *),
  285 (* PUNTO *),
  286 (* READ *),
  287 (* TEXTO *),
  288 (* THEN *),
  289 (* TIPO *),
  290 (* UNIT *),
    0];

val yylhs = "\255\255\
\\001\000\002\000\002\000\003\000\010\000\010\000\010\000\011\000\
\\012\000\012\000\004\000\004\000\004\000\004\000\004\000\004\000\
\\004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
\\004\000\004\000\004\000\004\000\004\000\004\000\005\000\005\000\
\\005\000\006\000\006\000\006\000\006\000\009\000\007\000\008\000\
\\008\000\000\000";

val yylen = "\002\000\
\\002\000\002\000\000\000\012\000\001\000\003\000\003\000\002\000\
\\003\000\000\000\001\000\001\000\001\000\001\000\003\000\003\000\
\\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\\001\000\003\000\003\000\006\000\005\000\001\000\001\000\003\000\
\\003\000\004\000\001\000\002\000\003\000\003\000\004\000\003\000\
\\000\000\002\000";

val yydefred = "\000\000\
\\000\000\000\000\035\000\031\000\000\000\000\000\011\000\000\000\
\\000\000\000\000\012\000\013\000\042\000\000\000\000\000\030\000\
\\025\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\036\000\000\000\000\000\000\000\001\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\002\000\000\000\026\000\038\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\032\000\000\000\000\000\000\000\
\\000\000\000\000\034\000\000\000\000\000\029\000\000\000\039\000\
\\000\000\000\000\000\000\000\000\005\000\000\000\040\000\000\000\
\\000\000\000\000\000\000\000\000\008\000\007\000\000\000\000\000\
\\000\000\000\000\009\000\000\000\000\000\004\000";

val yydgoto = "\002\000\
\\013\000\020\000\021\000\014\000\015\000\016\000\023\000\072\000\
\\017\000\078\000\081\000\085\000";

val yysindex = "\004\000\
\\117\000\000\000\000\000\000\000\117\000\252\254\000\000\117\000\
\\250\254\027\255\000\000\000\000\000\000\173\000\238\254\000\000\
\\000\000\104\000\041\255\249\254\252\254\148\000\030\255\117\000\
\\000\000\117\000\117\000\117\000\000\000\117\000\117\000\117\000\
\\117\000\117\000\117\000\117\000\117\000\117\000\045\255\117\000\
\\117\000\026\255\117\000\000\000\117\000\000\000\000\000\184\000\
\\042\001\029\255\000\000\029\255\010\255\029\255\029\255\029\255\
\\029\255\010\255\023\001\000\000\000\000\000\000\209\000\059\255\
\\233\000\244\000\000\000\117\000\067\255\000\000\117\000\000\000\
\\023\001\017\255\244\000\017\255\000\000\248\254\000\000\004\255\
\\050\255\017\255\073\255\017\255\000\000\000\000\000\000\017\255\
\\004\255\060\255\000\000\117\000\012\001\000\000";

val yyrindex = "\000\000\
\\000\000\000\000\000\000\000\000\000\000\071\255\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\079\000\000\000\
\\000\000\000\000\000\000\000\000\071\255\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\034\000\194\255\040\255\204\255\142\255\213\255\245\255\254\255\
\\008\000\168\255\043\000\076\255\000\000\109\255\000\000\000\000\
\\000\000\057\255\000\000\000\000\000\000\000\000\000\000\000\000\
\\053\000\000\000\057\255\000\000\000\000\000\000\000\000\063\255\
\\000\000\000\000\000\000\000\000\000\000\000\000\255\254\000\000\
\\063\255\000\000\000\000\000\000\000\000\000\000";

val yygindex = "\000\000\
\\000\000\068\000\000\000\251\255\000\000\000\000\000\000\016\000\
\\000\000\184\255\000\000\003\000";

val YYTABLESIZE = 581;
val yytable = "\018\000\
\\006\000\082\000\022\000\080\000\001\000\084\000\019\000\043\000\
\\006\000\087\000\039\000\089\000\006\000\082\000\028\000\090\000\
\\083\000\040\000\048\000\024\000\049\000\050\000\051\000\006\000\
\\052\000\053\000\054\000\055\000\056\000\057\000\058\000\059\000\
\\060\000\028\000\062\000\063\000\038\000\065\000\025\000\066\000\
\\018\000\018\000\076\000\018\000\018\000\031\000\018\000\018\000\
\\018\000\077\000\036\000\064\000\042\000\018\000\047\000\038\000\
\\018\000\018\000\018\000\018\000\018\000\018\000\073\000\018\000\
\\018\000\075\000\018\000\061\000\018\000\082\000\069\000\018\000\
\\074\000\092\000\086\000\018\000\017\000\017\000\088\000\017\000\
\\017\000\041\000\017\000\017\000\017\000\003\000\093\000\010\000\
\\044\000\017\000\079\000\091\000\017\000\017\000\017\000\017\000\
\\017\000\017\000\000\000\017\000\017\000\000\000\017\000\000\000\
\\017\000\000\000\000\000\017\000\000\000\033\000\033\000\017\000\
\\033\000\033\000\000\000\033\000\033\000\033\000\000\000\000\000\
\\000\000\000\000\033\000\000\000\000\000\033\000\033\000\033\000\
\\033\000\033\000\033\000\000\000\033\000\033\000\000\000\033\000\
\\000\000\033\000\000\000\000\000\033\000\000\000\015\000\015\000\
\\033\000\015\000\000\000\000\000\015\000\015\000\015\000\000\000\
\\000\000\000\000\000\000\015\000\000\000\000\000\015\000\015\000\
\\015\000\015\000\015\000\015\000\000\000\015\000\015\000\000\000\
\\016\000\016\000\015\000\016\000\000\000\015\000\016\000\016\000\
\\016\000\015\000\000\000\000\000\000\000\016\000\000\000\000\000\
\\016\000\016\000\016\000\016\000\016\000\016\000\000\000\016\000\
\\016\000\000\000\024\000\024\000\016\000\000\000\000\000\016\000\
\\024\000\024\000\024\000\016\000\019\000\019\000\000\000\000\000\
\\000\000\000\000\019\000\019\000\019\000\023\000\023\000\000\000\
\\000\000\024\000\024\000\023\000\023\000\023\000\024\000\000\000\
\\000\000\024\000\000\000\019\000\019\000\024\000\000\000\000\000\
\\019\000\000\000\000\000\019\000\023\000\023\000\000\000\019\000\
\\000\000\023\000\000\000\000\000\023\000\022\000\022\000\000\000\
\\023\000\000\000\000\000\022\000\022\000\022\000\021\000\021\000\
\\000\000\000\000\000\000\000\000\021\000\021\000\021\000\000\000\
\\020\000\020\000\000\000\000\000\022\000\022\000\020\000\020\000\
\\020\000\022\000\000\000\000\000\022\000\021\000\021\000\000\000\
\\022\000\000\000\021\000\000\000\000\000\021\000\000\000\020\000\
\\020\000\021\000\000\000\037\000\020\000\000\000\000\000\020\000\
\\037\000\037\000\037\000\020\000\027\000\000\000\000\000\000\000\
\\000\000\027\000\027\000\027\000\000\000\000\000\028\000\000\000\
\\000\000\037\000\037\000\028\000\028\000\028\000\037\000\000\000\
\\000\000\037\000\027\000\027\000\000\000\037\000\000\000\027\000\
\\000\000\000\000\027\000\000\000\028\000\028\000\027\000\014\000\
\\014\000\028\000\014\000\014\000\028\000\014\000\014\000\014\000\
\\028\000\000\000\000\000\000\000\014\000\000\000\000\000\014\000\
\\014\000\014\000\014\000\014\000\014\000\000\000\014\000\014\000\
\\026\000\014\000\000\000\027\000\028\000\000\000\014\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\030\000\000\000\003\000\
\\031\000\032\000\033\000\034\000\035\000\036\000\000\000\037\000\
\\004\000\005\000\038\000\000\000\006\000\000\000\000\000\041\000\
\\000\000\000\000\000\000\007\000\000\000\000\000\008\000\000\000\
\\009\000\000\000\010\000\011\000\026\000\045\000\012\000\027\000\
\\028\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\030\000\000\000\000\000\031\000\032\000\033\000\034\000\
\\035\000\036\000\000\000\037\000\046\000\026\000\038\000\000\000\
\\027\000\028\000\000\000\000\000\000\000\029\000\000\000\000\000\
\\026\000\000\000\030\000\027\000\028\000\031\000\032\000\033\000\
\\034\000\035\000\036\000\000\000\037\000\030\000\000\000\038\000\
\\031\000\032\000\033\000\034\000\035\000\036\000\000\000\037\000\
\\067\000\026\000\038\000\000\000\027\000\028\000\000\000\068\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\030\000\000\000\
\\000\000\031\000\032\000\033\000\034\000\035\000\036\000\000\000\
\\037\000\026\000\000\000\038\000\027\000\028\000\000\000\000\000\
\\070\000\000\000\000\000\000\000\026\000\071\000\030\000\027\000\
\\028\000\031\000\032\000\033\000\034\000\035\000\036\000\000\000\
\\037\000\030\000\000\000\038\000\031\000\032\000\033\000\034\000\
\\035\000\036\000\000\000\037\000\026\000\000\000\038\000\027\000\
\\028\000\000\000\000\000\094\000\000\000\000\000\000\000\026\000\
\\000\000\030\000\027\000\028\000\031\000\032\000\033\000\034\000\
\\035\000\036\000\000\000\037\000\030\000\000\000\038\000\031\000\
\\032\000\033\000\034\000\035\000\036\000\027\000\028\000\000\000\
\\000\000\038\000\000\000\000\000\000\000\000\000\000\000\030\000\
\\000\000\000\000\031\000\032\000\033\000\034\000\035\000\036\000\
\\000\000\000\000\000\000\000\000\038\000";

val yycheck = "\005\000\
\\002\001\010\001\008\000\076\000\001\000\002\001\011\001\015\001\
\\010\001\082\000\029\001\084\000\014\001\010\001\005\001\088\000\
\\025\001\036\001\024\000\026\001\026\000\027\000\028\000\025\001\
\\030\000\031\000\032\000\033\000\034\000\035\000\036\000\037\000\
\\038\000\005\001\040\000\041\000\027\001\043\000\012\001\045\000\
\\001\001\002\001\026\001\004\001\005\001\017\001\007\001\008\001\
\\009\001\033\001\022\001\026\001\012\001\014\001\025\001\027\001\
\\017\001\018\001\019\001\020\001\021\001\022\001\068\000\024\001\
\\025\001\071\000\027\001\023\001\029\001\010\001\012\001\032\001\
\\006\001\014\001\025\001\036\001\001\001\002\001\006\001\004\001\
\\005\001\025\001\007\001\008\001\009\001\015\001\092\000\025\001\
\\021\000\014\001\075\000\089\000\017\001\018\001\019\001\020\001\
\\021\001\022\001\255\255\024\001\025\001\255\255\027\001\255\255\
\\029\001\255\255\255\255\032\001\255\255\001\001\002\001\036\001\
\\004\001\005\001\255\255\007\001\008\001\009\001\255\255\255\255\
\\255\255\255\255\014\001\255\255\255\255\017\001\018\001\019\001\
\\020\001\021\001\022\001\255\255\024\001\025\001\255\255\027\001\
\\255\255\029\001\255\255\255\255\032\001\255\255\001\001\002\001\
\\036\001\004\001\255\255\255\255\007\001\008\001\009\001\255\255\
\\255\255\255\255\255\255\014\001\255\255\255\255\017\001\018\001\
\\019\001\020\001\021\001\022\001\255\255\024\001\025\001\255\255\
\\001\001\002\001\029\001\004\001\255\255\032\001\007\001\008\001\
\\009\001\036\001\255\255\255\255\255\255\014\001\255\255\255\255\
\\017\001\018\001\019\001\020\001\021\001\022\001\255\255\024\001\
\\025\001\255\255\001\001\002\001\029\001\255\255\255\255\032\001\
\\007\001\008\001\009\001\036\001\001\001\002\001\255\255\255\255\
\\255\255\255\255\007\001\008\001\009\001\001\001\002\001\255\255\
\\255\255\024\001\025\001\007\001\008\001\009\001\029\001\255\255\
\\255\255\032\001\255\255\024\001\025\001\036\001\255\255\255\255\
\\029\001\255\255\255\255\032\001\024\001\025\001\255\255\036\001\
\\255\255\029\001\255\255\255\255\032\001\001\001\002\001\255\255\
\\036\001\255\255\255\255\007\001\008\001\009\001\001\001\002\001\
\\255\255\255\255\255\255\255\255\007\001\008\001\009\001\255\255\
\\001\001\002\001\255\255\255\255\024\001\025\001\007\001\008\001\
\\009\001\029\001\255\255\255\255\032\001\024\001\025\001\255\255\
\\036\001\255\255\029\001\255\255\255\255\032\001\255\255\024\001\
\\025\001\036\001\255\255\002\001\029\001\255\255\255\255\032\001\
\\007\001\008\001\009\001\036\001\002\001\255\255\255\255\255\255\
\\255\255\007\001\008\001\009\001\255\255\255\255\002\001\255\255\
\\255\255\024\001\025\001\007\001\008\001\009\001\029\001\255\255\
\\255\255\032\001\024\001\025\001\255\255\036\001\255\255\029\001\
\\255\255\255\255\032\001\255\255\024\001\025\001\036\001\001\001\
\\002\001\029\001\004\001\005\001\032\001\007\001\008\001\009\001\
\\036\001\255\255\255\255\255\255\014\001\255\255\255\255\017\001\
\\018\001\019\001\020\001\021\001\022\001\255\255\024\001\025\001\
\\001\001\027\001\255\255\004\001\005\001\255\255\032\001\255\255\
\\255\255\255\255\255\255\255\255\255\255\014\001\255\255\003\001\
\\017\001\018\001\019\001\020\001\021\001\022\001\255\255\024\001\
\\012\001\013\001\027\001\255\255\016\001\255\255\255\255\032\001\
\\255\255\255\255\255\255\023\001\255\255\255\255\026\001\255\255\
\\028\001\255\255\030\001\031\001\001\001\002\001\034\001\004\001\
\\005\001\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\\255\255\014\001\255\255\255\255\017\001\018\001\019\001\020\001\
\\021\001\022\001\255\255\024\001\025\001\001\001\027\001\255\255\
\\004\001\005\001\255\255\255\255\255\255\009\001\255\255\255\255\
\\001\001\255\255\014\001\004\001\005\001\017\001\018\001\019\001\
\\020\001\021\001\022\001\255\255\024\001\014\001\255\255\027\001\
\\017\001\018\001\019\001\020\001\021\001\022\001\255\255\024\001\
\\025\001\001\001\027\001\255\255\004\001\005\001\255\255\007\001\
\\255\255\255\255\255\255\255\255\255\255\255\255\014\001\255\255\
\\255\255\017\001\018\001\019\001\020\001\021\001\022\001\255\255\
\\024\001\001\001\255\255\027\001\004\001\005\001\255\255\255\255\
\\008\001\255\255\255\255\255\255\001\001\002\001\014\001\004\001\
\\005\001\017\001\018\001\019\001\020\001\021\001\022\001\255\255\
\\024\001\014\001\255\255\027\001\017\001\018\001\019\001\020\001\
\\021\001\022\001\255\255\024\001\001\001\255\255\027\001\004\001\
\\005\001\255\255\255\255\008\001\255\255\255\255\255\255\001\001\
\\255\255\014\001\004\001\005\001\017\001\018\001\019\001\020\001\
\\021\001\022\001\255\255\024\001\014\001\255\255\027\001\017\001\
\\018\001\019\001\020\001\021\001\022\001\004\001\005\001\255\255\
\\255\255\027\001\255\255\255\255\255\255\255\255\255\255\014\001\
\\255\255\255\255\017\001\018\001\019\001\020\001\021\001\022\001\
\\255\255\255\255\255\255\255\255\027\001";

val yyact = vector_ 43 (fn () => ((raise Fail "parser") : obj));
(* Rule 1, file ffuncgrm.y, line 37 *)
val _ = update_ yyact 1
(fn () => repr(let
val d__1__ = peekVal 1 : ffuncast.Expr
in
( (d__1__) ) end : ffuncast.Expr))
;
(* Rule 2, file ffuncgrm.y, line 39 *)
val _ = update_ yyact 2
(fn () => repr(let
val d__1__ = peekVal 1 : ffuncast.Decl
val d__2__ = peekVal 0 : ffuncast.Decl list
in
( (d__1__)::(d__2__) ) end : ffuncast.Decl list))
;
(* Rule 3, file ffuncgrm.y, line 40 *)
val _ = update_ yyact 3
(fn () => repr(let
in
( [] ) end : ffuncast.Decl list))
;
(* Rule 4, file ffuncgrm.y, line 43 *)
val _ = update_ yyact 4
(fn () => repr(let
val d__2__ = peekVal 10 : string
val d__4__ = peekVal 8 : string
val d__6__ = peekVal 6 : ffunctipo.Tipo
val d__9__ = peekVal 3 : ffunctipo.Tipo
val d__11__ = peekVal 1 : ffuncast.Expr
in
( Func{f=(d__2__), arg=((d__4__), (d__6__)), body=(d__11__), tipo=(d__9__), pr=0} ) end : ffuncast.Decl))
;
(* Rule 5, file ffuncgrm.y, line 45 *)
val _ = update_ yyact 5
(fn () => repr(let
val d__1__ = peekVal 0 : ffunctipo.Tipo
in
( (d__1__) ) end : ffunctipo.Tipo))
;
(* Rule 6, file ffuncgrm.y, line 46 *)
val _ = update_ yyact 6
(fn () => repr(let
val d__1__ = peekVal 2 : ffunctipo.Tipo
val d__3__ = peekVal 0 : ffunctipo.Tipo
in
( TF((d__1__), (d__3__)) ) end : ffunctipo.Tipo))
;
(* Rule 7, file ffuncgrm.y, line 47 *)
val _ = update_ yyact 7
(fn () => repr(let
val d__2__ = peekVal 1 : ffunctipo.Tipo list
in
( TT (d__2__) ) end : ffunctipo.Tipo))
;
(* Rule 8, file ffuncgrm.y, line 49 *)
val _ = update_ yyact 8
(fn () => repr(let
val d__1__ = peekVal 1 : ffunctipo.Tipo
val d__2__ = peekVal 0 : ffunctipo.Tipo list
in
( (d__1__)::(d__2__) ) end : ffunctipo.Tipo list))
;
(* Rule 9, file ffuncgrm.y, line 51 *)
val _ = update_ yyact 9
(fn () => repr(let
val d__2__ = peekVal 1 : ffunctipo.Tipo
val d__3__ = peekVal 0 : ffunctipo.Tipo list
in
( (d__2__)::(d__3__) ) end : ffunctipo.Tipo list))
;
(* Rule 10, file ffuncgrm.y, line 52 *)
val _ = update_ yyact 10
(fn () => repr(let
in
( [] ) end : ffunctipo.Tipo list))
;
(* Rule 11, file ffuncgrm.y, line 54 *)
val _ = update_ yyact 11
(fn () => repr(let
val d__1__ = peekVal 0 : int
in
( Nro (d__1__) ) end : ffuncast.Expr))
;
(* Rule 12, file ffuncgrm.y, line 55 *)
val _ = update_ yyact 12
(fn () => repr(let
val d__1__ = peekVal 0 : string
in
( Texto (d__1__) ) end : ffuncast.Expr))
;
(* Rule 13, file ffuncgrm.y, line 56 *)
val _ = update_ yyact 13
(fn () => repr(let
in
( Unit ) end : ffuncast.Expr))
;
(* Rule 14, file ffuncgrm.y, line 57 *)
val _ = update_ yyact 14
(fn () => repr(let
val d__1__ = peekVal 0 : ffuncast.Expr
in
( (d__1__) ) end : ffuncast.Expr))
;
(* Rule 15, file ffuncgrm.y, line 58 *)
val _ = update_ yyact 15
(fn () => repr(let
val d__1__ = peekVal 2 : ffuncast.Expr
val d__3__ = peekVal 0 : ffuncast.Expr
in
( Oper(Suma, (d__1__), (d__3__)) ) end : ffuncast.Expr))
;
(* Rule 16, file ffuncgrm.y, line 59 *)
val _ = update_ yyact 16
(fn () => repr(let
val d__1__ = peekVal 2 : ffuncast.Expr
val d__3__ = peekVal 0 : ffuncast.Expr
in
( Oper(Resta, (d__1__), (d__3__)) ) end : ffuncast.Expr))
;
(* Rule 17, file ffuncgrm.y, line 60 *)
val _ = update_ yyact 17
(fn () => repr(let
val d__1__ = peekVal 2 : ffuncast.Expr
val d__3__ = peekVal 0 : ffuncast.Expr
in
( Oper(Prod, (d__1__), (d__3__)) ) end : ffuncast.Expr))
;
(* Rule 18, file ffuncgrm.y, line 61 *)
val _ = update_ yyact 18
(fn () => repr(let
val d__1__ = peekVal 2 : ffuncast.Expr
val d__3__ = peekVal 0 : ffuncast.Expr
in
( Oper(Div, (d__1__), (d__3__)) ) end : ffuncast.Expr))
;
(* Rule 19, file ffuncgrm.y, line 62 *)
val _ = update_ yyact 19
(fn () => repr(let
val d__1__ = peekVal 2 : ffuncast.Expr
val d__3__ = peekVal 0 : ffuncast.Expr
in
( Cmp(Igual, (d__1__), (d__3__)) ) end : ffuncast.Expr))
;
(* Rule 20, file ffuncgrm.y, line 63 *)
val _ = update_ yyact 20
(fn () => repr(let
val d__1__ = peekVal 2 : ffuncast.Expr
val d__3__ = peekVal 0 : ffuncast.Expr
in
( Cmp(Menor, (d__1__), (d__3__)) ) end : ffuncast.Expr))
;
(* Rule 21, file ffuncgrm.y, line 64 *)
val _ = update_ yyact 21
(fn () => repr(let
val d__1__ = peekVal 2 : ffuncast.Expr
val d__3__ = peekVal 0 : ffuncast.Expr
in
( Cmp(MenIg, (d__1__), (d__3__)) ) end : ffuncast.Expr))
;
(* Rule 22, file ffuncgrm.y, line 65 *)
val _ = update_ yyact 22
(fn () => repr(let
val d__1__ = peekVal 2 : ffuncast.Expr
val d__3__ = peekVal 0 : ffuncast.Expr
in
( Cmp(Mayor, (d__1__), (d__3__)) ) end : ffuncast.Expr))
;
(* Rule 23, file ffuncgrm.y, line 66 *)
val _ = update_ yyact 23
(fn () => repr(let
val d__1__ = peekVal 2 : ffuncast.Expr
val d__3__ = peekVal 0 : ffuncast.Expr
in
( Cmp(MayIg, (d__1__), (d__3__)) ) end : ffuncast.Expr))
;
(* Rule 24, file ffuncgrm.y, line 67 *)
val _ = update_ yyact 24
(fn () => repr(let
val d__1__ = peekVal 2 : ffuncast.Expr
val d__3__ = peekVal 0 : ffuncast.Expr
in
( Cmp(Dist, (d__1__), (d__3__)) ) end : ffuncast.Expr))
;
(* Rule 25, file ffuncgrm.y, line 68 *)
val _ = update_ yyact 25
(fn () => repr(let
val d__1__ = peekVal 0 : ffuncast.Expr list
in
( Tupla (d__1__) ) end : ffuncast.Expr))
;
(* Rule 26, file ffuncgrm.y, line 69 *)
val _ = update_ yyact 26
(fn () => repr(let
val d__2__ = peekVal 1 : ffuncast.Expr
in
( (d__2__) ) end : ffuncast.Expr))
;
(* Rule 27, file ffuncgrm.y, line 70 *)
val _ = update_ yyact 27
(fn () => repr(let
val d__1__ = peekVal 2 : ffuncast.Expr
val d__3__ = peekVal 0 : ffuncast.Expr
in
( Sec((d__1__), (d__3__)) ) end : ffuncast.Expr))
;
(* Rule 28, file ffuncgrm.y, line 71 *)
val _ = update_ yyact 28
(fn () => repr(let
val d__2__ = peekVal 4 : ffuncast.Expr
val d__4__ = peekVal 2 : ffuncast.Expr
val d__6__ = peekVal 0 : ffuncast.Expr
in
( If((d__2__), (d__4__), (d__6__)) ) end : ffuncast.Expr))
;
(* Rule 29, file ffuncgrm.y, line 72 *)
val _ = update_ yyact 29
(fn () => repr(let
val d__2__ = peekVal 3 : ffuncast.Decl list
val d__4__ = peekVal 1 : ffuncast.Expr
in
( Let((d__2__), (d__4__)) ) end : ffuncast.Expr))
;
(* Rule 30, file ffuncgrm.y, line 73 *)
val _ = update_ yyact 30
(fn () => repr(let
val d__1__ = peekVal 0 : ffuncast.Expr
in
( (d__1__) ) end : ffuncast.Expr))
;
(* Rule 31, file ffuncgrm.y, line 75 *)
val _ = update_ yyact 31
(fn () => repr(let
val d__1__ = peekVal 0 : string
in
( Id (d__1__) ) end : ffuncast.Expr))
;
(* Rule 32, file ffuncgrm.y, line 76 *)
val _ = update_ yyact 32
(fn () => repr(let
val d__1__ = peekVal 2 : ffuncast.Expr
val d__3__ = peekVal 0 : int
in
( Memb((d__1__), (d__3__)) ) end : ffuncast.Expr))
;
(* Rule 33, file ffuncgrm.y, line 77 *)
val _ = update_ yyact 33
(fn () => repr(let
val d__1__ = peekVal 2 : ffuncast.Expr
val d__3__ = peekVal 0 : ffuncast.Expr
in
( Call((d__3__), (d__1__)) ) end : ffuncast.Expr))
;
(* Rule 34, file ffuncgrm.y, line 79 *)
val _ = update_ yyact 34
(fn () => repr(let
val d__3__ = peekVal 1 : ffuncast.Expr
in
( Print (d__3__) ) end : ffuncast.Expr))
;
(* Rule 35, file ffuncgrm.y, line 80 *)
val _ = update_ yyact 35
(fn () => repr(let
in
( Cr ) end : ffuncast.Expr))
;
(* Rule 36, file ffuncgrm.y, line 81 *)
val _ = update_ yyact 36
(fn () => repr(let
val d__2__ = peekVal 0 : string
in
( Read (d__2__) ) end : ffuncast.Expr))
;
(* Rule 37, file ffuncgrm.y, line 82 *)
val _ = update_ yyact 37
(fn () => repr(let
val d__1__ = peekVal 2 : ffuncast.Expr
val d__3__ = peekVal 0 : ffuncast.Expr
in
( Asign((d__1__), (d__3__)) ) end : ffuncast.Expr))
;
(* Rule 38, file ffuncgrm.y, line 84 *)
val _ = update_ yyact 38
(fn () => repr(let
val d__2__ = peekVal 1 : ffuncast.Expr list
in
( (d__2__) ) end : ffuncast.Expr list))
;
(* Rule 39, file ffuncgrm.y, line 86 *)
val _ = update_ yyact 39
(fn () => repr(let
val d__1__ = peekVal 3 : ffuncast.Expr
val d__3__ = peekVal 1 : ffuncast.Expr
val d__4__ = peekVal 0 : ffuncast.Expr list
in
( (d__1__)::(d__3__)::(d__4__) ) end : ffuncast.Expr list))
;
(* Rule 40, file ffuncgrm.y, line 88 *)
val _ = update_ yyact 40
(fn () => repr(let
val d__2__ = peekVal 1 : ffuncast.Expr
val d__3__ = peekVal 0 : ffuncast.Expr list
in
( (d__2__)::(d__3__) ) end : ffuncast.Expr list))
;
(* Rule 41, file ffuncgrm.y, line 89 *)
val _ = update_ yyact 41
(fn () => repr(let
in
( [] ) end : ffuncast.Expr list))
;
(* Entry prog *)
val _ = update_ yyact 42 (fn () => raise yyexit (peekVal 0));
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