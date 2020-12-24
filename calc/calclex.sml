local open Obj Lexing in


open calcgrm

fun atoi s = valOf(Int.fromString s)

fun action_10 lexbuf = (
 NRO(atoi(getLexeme lexbuf)) )
and action_9 lexbuf = (
 NRO(atoi(getLexeme lexbuf)) )
and action_8 lexbuf = (
 PDER )
and action_7 lexbuf = (
 PIZQ )
and action_6 lexbuf = (
 DIV )
and action_5 lexbuf = (
 POR )
and action_4 lexbuf = (
 MENOS )
and action_3 lexbuf = (
 MAS )
and action_2 lexbuf = (
 PCOMA )
and action_1 lexbuf = (
 Tok lexbuf )
and action_0 lexbuf = (
 EOF )
and state_0 lexbuf = (
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_9 lexbuf
 else case currChar of
    #"\n" => action_1 lexbuf
 |  #"\t" => action_1 lexbuf
 |  #" " => action_1 lexbuf
 |  #";" => action_2 lexbuf
 |  #"/" => action_6 lexbuf
 |  #"-" => state_7 lexbuf
 |  #"+" => action_3 lexbuf
 |  #"*" => action_5 lexbuf
 |  #")" => action_8 lexbuf
 |  #"(" => action_7 lexbuf
 |  #"\^@" => action_0 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_7 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_4);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_11 lexbuf
 else backtrack lexbuf
 end)
and state_9 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_9);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_9 lexbuf
 else backtrack lexbuf
 end)
and state_11 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_10);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_11 lexbuf
 else backtrack lexbuf
 end)
and Tok lexbuf =
  (setLexLastAction lexbuf (magic dummyAction);
   setLexStartPos lexbuf (getLexCurrPos lexbuf);
   state_0 lexbuf)

(* The following checks type consistency of actions *)
val _ = fn _ => [action_10, action_9, action_8, action_7, action_6, action_5, action_4, action_3, action_2, action_1, action_0];

end
