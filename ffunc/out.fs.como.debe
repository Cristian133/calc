: __f0 recursive
{ x y }
2 x *
 y *
;
: __f1 recursive
{ x }
x ['] __f0
;
: f recursive
{ a b c }
c b a execute
 execute
;
' __f1 55 66 f
 . cr
bye
