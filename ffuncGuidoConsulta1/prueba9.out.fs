create _input-buffer 80 chars allot
: getint
begin _input-buffer 80 accept
_input-buffer swap
s>number?  invert
while
." Ingrese nuevamente" cr
2drop
repeat drop ;
: tupla ( n1 n2 ... u -- t )
  dup here swap cells allot 
  swap
     0 swap -do
     dup i 1- cells + rot swap !
  1 -loop
;
defer _f
:noname { _sl _x }
_x 0 cells + @
. cr
_x 1 cells + @
_x 2 cells + @
type cr
_x 3 cells + @
. cr
;
is _f
0 ( sl ) 100 s" hola" 200 4 tupla
' _f execute
bye
