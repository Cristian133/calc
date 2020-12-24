: _mkstring
  here 2 cells allot dup 3 roll 3 roll rot 2!
;
: _mktuple ( eu .. e1 u -- tupla )
dup here swap cells allot swap
0 do
  dup i cells + rot swap !
  loop
;

\ (1,("hola",3),"chau",5)

5 s" chau" _mkstring .s cr
3 s" hola" _mkstring .s cr
2 _mktuple .s cr
1 4 _mktuple .s cr
\ la direccion de la tupla en el stack
dup
@ . cr \ 1
dup
\ 1 cells + @ .s cr @ .s cr 2@ .s cr
1 cells + @ @ 2@ type cr \ hola
dup
1 cells + @ 1 cells + @ . cr
dup
2 cells + @ 2@ type cr \ chau
dup
3 cells + @ . cr
bye
