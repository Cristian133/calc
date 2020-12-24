: @clausura ( xt an ... a0 n -- clausura )
  dup 1+ here swap cells allot ( an .. a0 n addr )
  over 1+ 0 ( xt an ... a0 n addr n+1 0 )
  +do
     dup i cells + rot swap !
  loop
;
: @despliega  ( addr )
  dup @ 0 ( addr n 0 )
  +do
    dup i 1+ cells +
    @ swap
  loop
  drop
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
