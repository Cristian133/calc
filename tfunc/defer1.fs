defer f
defer g
:noname begin dup g dup 0> while dup . cr repeat ;
is f
:noname 1- ;
is g

10 f .
bye
