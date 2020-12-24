: _mkstring
  here 2 cells allot dup 3 roll 3 roll rot 2!
;
: _mktuple ( eu .. e1 u -- tupla )
dup here swap cells allot swap
0 do
  dup i cells + rot swap s" -> " type .s cr !
  loop
;
s" Hola mundo!" .s cr _mkstring .s cr 123 .s cr s" Adios mundo!" .s cr _mkstring .s cr 3 _mktuple .s cr

dup @ 2@ type cr
dup 1 cells + @ . cr
dup 2 cells + @ 2@ type cr

dup 0 cells + @ .s cr 2@ rot .s cr dup .s cr 1 cells + .s cr @ .s cr swap dup 2 cells + @ 2@ rot drop .s cr
type cr . type cr
bye

