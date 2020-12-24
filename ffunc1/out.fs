create _input-buffer 80 chars allot
: getint
begin _input-buffer 80 accept
_input-buffer swap
s>number?  invert
while
." Ingrese nuevamente" cr
2drop
repeat drop ;
defer ___f0
defer f
:noname { _x }
s" hola"  type 

 cr

;
is ___f0
:noname { _x }
0  _x  execute
;
is f
 ' ___f0   f

bye
