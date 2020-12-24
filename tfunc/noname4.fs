defer f
defer g
:noname dup 0> if g endif ;
is f
:noname dup . 1- f ;
is g

100 f bye
