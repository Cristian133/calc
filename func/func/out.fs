create _input-buffer 80 chars allot
: getint
begin _input-buffer 80 accept
_input-buffer swap
s>number?  invert
while
." Ingrese nuevamente" cr
2drop
repeat drop ;
: f recursive
{ _n }
getint to _n
_n ;
0 f
 . 
 cr
bye
