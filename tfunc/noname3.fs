:noname .s cr { x y z } x . y . z . cr ;
create f1 ,
f1 . cr
f1 @ . cr
\ :noname .s cr { x y } 10 x y ['] f1 .s cr execute ;
:noname .s cr { x y } 10 x y f1 @ .s cr execute ;
create f2 ,
.s cr
:noname { x } 11 x f2 @ execute ;
create f3 ,
12 f3 @ execute
bye
