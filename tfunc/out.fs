create _input-buffer 80 chars allot
: getint
begin _input-buffer 80 accept
_input-buffer swap
s>number?  invert
while
." Ingrese nuevamente" cr
2drop
repeat drop ;
defer f
defer g
:noname { _x _y _z }
_x  _y  +
 _z  +
;
is f
:noname { _x }
2 _x  *
;
is g
1 2 3 f
 .
 cr
4 g
 .
 cr

bye
