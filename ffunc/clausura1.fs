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
:noname + * ;  10 20 30 4 .s cr  @clausura
dup 5 cells dump cr
dup
cr cr
.s cr
@despliega .s cr execute
 .s cr
drop
.s cr
@execute
.s cr
variable funcion
:noname { x y z } x . cr y . cr z . cr ;
\ .s cr
10 11 12 4 @clausura funcion !
.s cr 
funcion @ .s cr
@execute

12 11 10 :noname { x y z } x . cr y . cr z . cr ; .s cr execute
bye
