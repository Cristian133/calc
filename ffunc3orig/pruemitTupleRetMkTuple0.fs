: _mkstring
  here 2 cells allot dup 3 roll 3 roll rot 2!
;
: _mktuple ( eu .. e1 u -- tupla )
dup here swap cells allot swap
0 do
  dup i cells + rot swap !
  loop
;
s" Hola mundo!" .s cr _mkstring
123 s" Adios mundo!" .s cr _mkstring 3 _mktuple

dup @ 2@ type cr
dup 1 cells + @ . cr
dup 2 cells + @ 2@ type cr

dup 2 cells + @ 2@ .s cr rot dup 1 cells + @ .s cr swap dup 0 cells + @ 2@ rot .s cr drop

type cr
. cr
type cr

bye

