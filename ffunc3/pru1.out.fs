create _input-buffer 80 chars allot
: getint
begin _input-buffer 80 accept
_input-buffer swap
s>number?  invert
while
." Ingrese nuevamente" cr
2drop
repeat drop ;
: _slink ( lp leveldif -- lp )
  0 u+do 1 cells + @ loop
;
: _slink-cell ( lp leveldif -- cell )
  _slink 2 cells + @
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
defer _f.1
defer _g.0
:noname { _h _sl _x }
_x . cr
_x 1 cells + @ @ 2@
type cr
_x 2 cells + @ . cr
_x 3 cells + @ @ 2@
type cr
_h here - allot
;
is _f.1
:noname { _h _sl _s }
0 _h here - allot
;
is _g.0
 here lp@ s" defg" _mkstring
2 s" abc" _mkstring
1 4 _mktuple
_f.1
bye
