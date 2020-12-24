local open Obj Lexing in


open funcgrm

fun atoi s = (valOf o Int.fromString) s

fun action_11 lexbuf = (
 NRO(atoi(getLexeme lexbuf)) )
and action_10 lexbuf = (
 ID(getLexeme lexbuf) )
and action_9 lexbuf = (
 END )
and action_8 lexbuf = (
 FUN )
and action_7 lexbuf = (
 DOSP )
and action_6 lexbuf = (
 COMA )
and action_5 lexbuf = (
 POR )
and action_4 lexbuf = (
 MAS )
and action_3 lexbuf = (
 PD )
and action_2 lexbuf = (
 PI )
and action_1 lexbuf = (
 Tok lexbuf )
and action_0 lexbuf = (
 EOF )
and state_0 lexbuf = (
 let val currChar = getNextChar lexbuf in
 if currChar >= #"g" andalso currChar <= #"z" then  state_10 lexbuf
 else if currChar >= #"0" andalso currChar <= #"9" then  state_8 lexbuf
 else case currChar of
    #"d" => state_10 lexbuf
 |  #"c" => state_10 lexbuf
 |  #"b" => state_10 lexbuf
 |  #"a" => state_10 lexbuf
 |  #"\n" => action_1 lexbuf
 |  #"\t" => action_1 lexbuf
 |  #" " => action_1 lexbuf
 |  #"f" => state_12 lexbuf
 |  #"e" => state_11 lexbuf
 |  #":" => action_7 lexbuf
 |  #"," => action_6 lexbuf
 |  #"+" => action_4 lexbuf
 |  #"*" => action_5 lexbuf
 |  #")" => action_3 lexbuf
 |  #"(" => action_2 lexbuf
 |  #"\^@" => action_0 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_8 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_11);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_8 lexbuf
 else backtrack lexbuf
 end)
and state_10 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_10);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"a" andalso currChar <= #"z" then  state_10 lexbuf
 else backtrack lexbuf
 end)
and state_11 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_10);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"a" andalso currChar <= #"m" then  state_10 lexbuf
 else if currChar >= #"o" andalso currChar <= #"z" then  state_10 lexbuf
 else case currChar of
    #"n" => state_15 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_12 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_10);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"a" andalso currChar <= #"t" then  state_10 lexbuf
 else case currChar of
    #"z" => state_10 lexbuf
 |  #"y" => state_10 lexbuf
 |  #"x" => state_10 lexbuf
 |  #"w" => state_10 lexbuf
 |  #"v" => state_10 lexbuf
 |  #"u" => state_13 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_13 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_10);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"a" andalso currChar <= #"m" then  state_10 lexbuf
 else if currChar >= #"o" andalso currChar <= #"z" then  state_10 lexbuf
 else case currChar of
    #"n" => state_14 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_14 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_8);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"a" andalso currChar <= #"z" then  state_10 lexbuf
 else backtrack lexbuf
 end)
and state_15 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_10);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"e" andalso currChar <= #"z" then  state_10 lexbuf
 else case currChar of
    #"c" => state_10 lexbuf
 |  #"b" => state_10 lexbuf
 |  #"a" => state_10 lexbuf
 |  #"d" => state_16 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_16 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_9);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"a" andalso currChar <= #"z" then  state_10 lexbuf
 else backtrack lexbuf
 end)
and Tok lexbuf =
  (setLexLastAction lexbuf (magic dummyAction);
   setLexStartPos lexbuf (getLexCurrPos lexbuf);
   state_0 lexbuf)

(* The following checks type consistency of actions *)
val _ = fn _ => [action_11, action_10, action_9, action_8, action_7, action_6, action_5, action_4, action_3, action_2, action_1, action_0];

end
