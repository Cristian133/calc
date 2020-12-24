: _mkstring
  here 2 cells allot dup 3 roll 3 roll rot 2!
;
: _mktuple ( eu .. e1 u -- tupla )
dup here swap cells allot swap
0 do
  dup i cells + rot swap !
  loop
;

\ (123, ("hola", ("chau", 125), 124), "hasta nunca")
s" hasta nunca" _mkstring
124
125 s" chau" _mkstring 2 _mktuple
s" hola" _mkstring 3 _mktuple
123 3 _mktuple
.s cr
dup 0 cells + @ swap
dup 1 cells + @ \ tupla
  .s cr
  dup 0 cells + @ 2@ rot
  dup 1 cells + @ \ tupla
    dup 0 cells + @ 2@ rot
    dup 1 cells + @ swap
    drop
  .s cr
    3 roll
  .s cr
  dup 2 cells + @ swap
  drop
  4 roll
dup 2 cells + @ 2@ rot
drop
type cr . cr . cr type cr type cr . cr
bye
