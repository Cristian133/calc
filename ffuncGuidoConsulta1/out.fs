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
defer _f.0
:noname { _h _sl _x }
125 124 s" chau" _mkstring
s" hola" _mkstring
3 _mktuple
123 3 _mktuple
dup 2 cells + @ swap ( int )
dup 1 cells + @ ( tupla )
dup 2 cells + @ swap ( int )
dup 1 cells + @ 2@ rot ( string )
dup 0 cells + @ 2@ rot ( string )
drop
5 roll
dup 0 cells + @ swap ( int )
drop
_h here - allot
;
is _f.0
 here lp@ 0 _f.0 here 3 cells allot dup -rot 0 cells + ! here 3 cells allot 3 roll 3 roll _mkString swap dup -rot 0 cells + ! 3 roll 3 roll _mkString swap dup -rot 1 cells + ! 2 roll over 2 cells + ! swap dup rot 1 cells + ! 2 cells + ! 1 cells + @ 1 cells + @ @ 2@
type cr
bye
