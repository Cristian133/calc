create _input-buffer 80 chars allot
: getint
begin _input-buffer 80 accept
_input-buffer swap
s>number?  invert
while
." Ingrese nuevamente" cr
2drop
repeat drop ;
: _tupla ( n1 n2 ... u -- t )
  dup here swap cells allot 
  swap
     0 swap -do
     dup i 1- cells + rot swap !
  1 -loop
;
: _slink ( lp leveldif -- lp )
  0 u+do @ loop
;
: cells+@ cells + @ ;
: _slink-cell ( off lp leveldif -- cell )
  _slink swap cells+@
;
: _slink-2cell ( off lp leveldif -- 2cell )
  _slink swap over over 1+ cells+@ -rot cells+@
;
: _mkstring
  here 2 cells allot dup 3 roll 3 roll rot 2!
;
: _mktuple ( eu .. e1 u -- tupla )
dup here swap cells allot swap
0 do
  dup i cells + rot swap !
  loop
;
defer _f
:noname { _h _sl _x }
_x 1 cells + @
0 cells + @ 2@ ( acaaaaaaaaaaa )
type cr
_h here - allot
;
is _f
here lp@ ( sl ) 5 s" chau" _mkstring
3 s" hola" _mkstring
2 _mktuple
1 4 _mktuple
' _f execute
bye
