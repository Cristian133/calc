: @clausura ( xt an ... a0 n -- clausura )
  dup 1+ here swap cells allot ( an .. a0 n addr ) .s cr
  dup { x }
  over 1+ 0 ( xt an ... a0 n addr n 0 ) .s cr
  +do
     x i .s cr
     cells + .s cr
     rot swap ! .s cr
  loop
;
: @despliega  ( addr )
  dup @ 0 ( addr n 0 ) .s cr
  +do
    dup i 1+ cells + .s cr @ swap
  loop
  drop
  .s cr
;
: @execute @despliega execute ;


\ prueba
:noname + * + + + ;
1 2 3 4 3 2 7 @clausura
dup 8 cells dump cr
.s cr
cr cr
@despliega
execute
.s cr
:noname { x y z } x . cr y . cr z . cr ;
11 12 13 4 @clausura
@execute
bye
