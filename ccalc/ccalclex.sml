local open Obj Lexing in


open ccalcgrm

fun atoi s = valOf(Int.fromString s)

fun action_25 lexbuf = (
 VAR(getLexeme lexbuf) )
and action_24 lexbuf = (
 NRO(atoi(getLexeme lexbuf)) )
and action_23 lexbuf = (
 NRO(atoi(getLexeme lexbuf)) )
and action_22 lexbuf = (
 PRINT )
and action_21 lexbuf = (
 END )
and action_20 lexbuf = (
 DO )
and action_19 lexbuf = (
 WHILE )
and action_18 lexbuf = (
 ELSE )
and action_17 lexbuf = (
 THEN )
and action_16 lexbuf = (
 IF )
and action_15 lexbuf = (
 ASIGN )
and action_14 lexbuf = (
 PDER )
and action_13 lexbuf = (
 PIZQ )
and action_12 lexbuf = (
 DIST )
and action_11 lexbuf = (
 IGUAL )
and action_10 lexbuf = (
 MAYIG )
and action_9 lexbuf = (
 MAYOR )
and action_8 lexbuf = (
 MENIG )
and action_7 lexbuf = (
 MENOR )
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
 if currChar >= #"A" andalso currChar <= #"Z" then  state_14 lexbuf
 else if currChar >= #"j" andalso currChar <= #"o" then  state_14 lexbuf
 else if currChar >= #"0" andalso currChar <= #"9" then  state_9 lexbuf
 else case currChar of
    #"c" => state_14 lexbuf
 |  #"b" => state_14 lexbuf
 |  #"a" => state_14 lexbuf
 |  #"h" => state_14 lexbuf
 |  #"g" => state_14 lexbuf
 |  #"f" => state_14 lexbuf
 |  #"s" => state_14 lexbuf
 |  #"r" => state_14 lexbuf
 |  #"q" => state_14 lexbuf
 |  #"v" => state_14 lexbuf
 |  #"u" => state_14 lexbuf
 |  #"z" => state_14 lexbuf
 |  #"y" => state_14 lexbuf
 |  #"x" => state_14 lexbuf
 |  #"\n" => action_1 lexbuf
 |  #"\t" => action_1 lexbuf
 |  #" " => action_1 lexbuf
 |  #"w" => state_20 lexbuf
 |  #"t" => state_19 lexbuf
 |  #"p" => state_18 lexbuf
 |  #"i" => state_17 lexbuf
 |  #"e" => state_16 lexbuf
 |  #"d" => state_15 lexbuf
 |  #">" => state_13 lexbuf
 |  #"=" => action_11 lexbuf
 |  #"<" => state_11 lexbuf
 |  #";" => action_2 lexbuf
 |  #"/" => action_6 lexbuf
 |  #"-" => state_7 lexbuf
 |  #"+" => action_3 lexbuf
 |  #"*" => action_5 lexbuf
 |  #")" => action_14 lexbuf
 |  #"(" => action_13 lexbuf
 |  #"\^@" => action_0 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_7 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_4);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_43 lexbuf
 else backtrack lexbuf
 end)
and state_9 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_23);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_9 lexbuf
 else backtrack lexbuf
 end)
and state_11 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_7);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #">" => action_12 lexbuf
 |  #"=" => action_8 lexbuf
 |  #"-" => action_15 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_13 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_9);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"=" => action_10 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_14 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_25);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_14 lexbuf
 else if currChar >= #"A" andalso currChar <= #"Z" then  state_14 lexbuf
 else if currChar >= #"a" andalso currChar <= #"z" then  state_14 lexbuf
 else case currChar of
    #"_" => state_14 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_15 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_25);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_14 lexbuf
 else if currChar >= #"A" andalso currChar <= #"Z" then  state_14 lexbuf
 else if currChar >= #"a" andalso currChar <= #"n" then  state_14 lexbuf
 else if currChar >= #"p" andalso currChar <= #"z" then  state_14 lexbuf
 else case currChar of
    #"_" => state_14 lexbuf
 |  #"o" => state_38 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_16 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_25);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_14 lexbuf
 else if currChar >= #"A" andalso currChar <= #"Z" then  state_14 lexbuf
 else if currChar >= #"a" andalso currChar <= #"k" then  state_14 lexbuf
 else if currChar >= #"o" andalso currChar <= #"z" then  state_14 lexbuf
 else case currChar of
    #"_" => state_14 lexbuf
 |  #"m" => state_14 lexbuf
 |  #"n" => state_34 lexbuf
 |  #"l" => state_33 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_17 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_25);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_14 lexbuf
 else if currChar >= #"A" andalso currChar <= #"Z" then  state_14 lexbuf
 else if currChar >= #"g" andalso currChar <= #"z" then  state_14 lexbuf
 else case currChar of
    #"_" => state_14 lexbuf
 |  #"e" => state_14 lexbuf
 |  #"d" => state_14 lexbuf
 |  #"c" => state_14 lexbuf
 |  #"b" => state_14 lexbuf
 |  #"a" => state_14 lexbuf
 |  #"f" => state_32 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_18 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_25);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_14 lexbuf
 else if currChar >= #"A" andalso currChar <= #"Z" then  state_14 lexbuf
 else if currChar >= #"a" andalso currChar <= #"q" then  state_14 lexbuf
 else if currChar >= #"s" andalso currChar <= #"z" then  state_14 lexbuf
 else case currChar of
    #"_" => state_14 lexbuf
 |  #"r" => state_28 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_19 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_25);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_14 lexbuf
 else if currChar >= #"A" andalso currChar <= #"Z" then  state_14 lexbuf
 else if currChar >= #"a" andalso currChar <= #"g" then  state_14 lexbuf
 else if currChar >= #"i" andalso currChar <= #"z" then  state_14 lexbuf
 else case currChar of
    #"_" => state_14 lexbuf
 |  #"h" => state_25 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_20 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_25);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_14 lexbuf
 else if currChar >= #"A" andalso currChar <= #"Z" then  state_14 lexbuf
 else if currChar >= #"a" andalso currChar <= #"g" then  state_14 lexbuf
 else if currChar >= #"i" andalso currChar <= #"z" then  state_14 lexbuf
 else case currChar of
    #"_" => state_14 lexbuf
 |  #"h" => state_21 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_21 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_25);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_14 lexbuf
 else if currChar >= #"A" andalso currChar <= #"Z" then  state_14 lexbuf
 else if currChar >= #"a" andalso currChar <= #"h" then  state_14 lexbuf
 else if currChar >= #"j" andalso currChar <= #"z" then  state_14 lexbuf
 else case currChar of
    #"_" => state_14 lexbuf
 |  #"i" => state_22 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_22 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_25);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_14 lexbuf
 else if currChar >= #"A" andalso currChar <= #"Z" then  state_14 lexbuf
 else if currChar >= #"a" andalso currChar <= #"k" then  state_14 lexbuf
 else if currChar >= #"m" andalso currChar <= #"z" then  state_14 lexbuf
 else case currChar of
    #"_" => state_14 lexbuf
 |  #"l" => state_23 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_23 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_25);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_14 lexbuf
 else if currChar >= #"A" andalso currChar <= #"Z" then  state_14 lexbuf
 else if currChar >= #"f" andalso currChar <= #"z" then  state_14 lexbuf
 else case currChar of
    #"_" => state_14 lexbuf
 |  #"d" => state_14 lexbuf
 |  #"c" => state_14 lexbuf
 |  #"b" => state_14 lexbuf
 |  #"a" => state_14 lexbuf
 |  #"e" => state_24 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_24 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_19);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_14 lexbuf
 else if currChar >= #"A" andalso currChar <= #"Z" then  state_14 lexbuf
 else if currChar >= #"a" andalso currChar <= #"z" then  state_14 lexbuf
 else case currChar of
    #"_" => state_14 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_25 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_25);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_14 lexbuf
 else if currChar >= #"A" andalso currChar <= #"Z" then  state_14 lexbuf
 else if currChar >= #"f" andalso currChar <= #"z" then  state_14 lexbuf
 else case currChar of
    #"_" => state_14 lexbuf
 |  #"d" => state_14 lexbuf
 |  #"c" => state_14 lexbuf
 |  #"b" => state_14 lexbuf
 |  #"a" => state_14 lexbuf
 |  #"e" => state_26 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_26 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_25);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_14 lexbuf
 else if currChar >= #"A" andalso currChar <= #"Z" then  state_14 lexbuf
 else if currChar >= #"a" andalso currChar <= #"m" then  state_14 lexbuf
 else if currChar >= #"o" andalso currChar <= #"z" then  state_14 lexbuf
 else case currChar of
    #"_" => state_14 lexbuf
 |  #"n" => state_27 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_27 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_17);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_14 lexbuf
 else if currChar >= #"A" andalso currChar <= #"Z" then  state_14 lexbuf
 else if currChar >= #"a" andalso currChar <= #"z" then  state_14 lexbuf
 else case currChar of
    #"_" => state_14 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_28 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_25);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_14 lexbuf
 else if currChar >= #"A" andalso currChar <= #"Z" then  state_14 lexbuf
 else if currChar >= #"a" andalso currChar <= #"h" then  state_14 lexbuf
 else if currChar >= #"j" andalso currChar <= #"z" then  state_14 lexbuf
 else case currChar of
    #"_" => state_14 lexbuf
 |  #"i" => state_29 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_29 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_25);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_14 lexbuf
 else if currChar >= #"A" andalso currChar <= #"Z" then  state_14 lexbuf
 else if currChar >= #"a" andalso currChar <= #"m" then  state_14 lexbuf
 else if currChar >= #"o" andalso currChar <= #"z" then  state_14 lexbuf
 else case currChar of
    #"_" => state_14 lexbuf
 |  #"n" => state_30 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_30 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_25);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_14 lexbuf
 else if currChar >= #"A" andalso currChar <= #"Z" then  state_14 lexbuf
 else if currChar >= #"a" andalso currChar <= #"s" then  state_14 lexbuf
 else if currChar >= #"u" andalso currChar <= #"z" then  state_14 lexbuf
 else case currChar of
    #"_" => state_14 lexbuf
 |  #"t" => state_31 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_31 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_22);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_14 lexbuf
 else if currChar >= #"A" andalso currChar <= #"Z" then  state_14 lexbuf
 else if currChar >= #"a" andalso currChar <= #"z" then  state_14 lexbuf
 else case currChar of
    #"_" => state_14 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_32 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_16);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_14 lexbuf
 else if currChar >= #"A" andalso currChar <= #"Z" then  state_14 lexbuf
 else if currChar >= #"a" andalso currChar <= #"z" then  state_14 lexbuf
 else case currChar of
    #"_" => state_14 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_33 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_25);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_14 lexbuf
 else if currChar >= #"A" andalso currChar <= #"Z" then  state_14 lexbuf
 else if currChar >= #"a" andalso currChar <= #"r" then  state_14 lexbuf
 else if currChar >= #"t" andalso currChar <= #"z" then  state_14 lexbuf
 else case currChar of
    #"_" => state_14 lexbuf
 |  #"s" => state_36 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_34 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_25);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_14 lexbuf
 else if currChar >= #"A" andalso currChar <= #"Z" then  state_14 lexbuf
 else if currChar >= #"e" andalso currChar <= #"z" then  state_14 lexbuf
 else case currChar of
    #"_" => state_14 lexbuf
 |  #"c" => state_14 lexbuf
 |  #"b" => state_14 lexbuf
 |  #"a" => state_14 lexbuf
 |  #"d" => state_35 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_35 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_21);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_14 lexbuf
 else if currChar >= #"A" andalso currChar <= #"Z" then  state_14 lexbuf
 else if currChar >= #"a" andalso currChar <= #"z" then  state_14 lexbuf
 else case currChar of
    #"_" => state_14 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_36 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_25);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_14 lexbuf
 else if currChar >= #"A" andalso currChar <= #"Z" then  state_14 lexbuf
 else if currChar >= #"f" andalso currChar <= #"z" then  state_14 lexbuf
 else case currChar of
    #"_" => state_14 lexbuf
 |  #"d" => state_14 lexbuf
 |  #"c" => state_14 lexbuf
 |  #"b" => state_14 lexbuf
 |  #"a" => state_14 lexbuf
 |  #"e" => state_37 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_37 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_18);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_14 lexbuf
 else if currChar >= #"A" andalso currChar <= #"Z" then  state_14 lexbuf
 else if currChar >= #"a" andalso currChar <= #"z" then  state_14 lexbuf
 else case currChar of
    #"_" => state_14 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_38 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_20);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_14 lexbuf
 else if currChar >= #"A" andalso currChar <= #"Z" then  state_14 lexbuf
 else if currChar >= #"a" andalso currChar <= #"z" then  state_14 lexbuf
 else case currChar of
    #"_" => state_14 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_43 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_24);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_43 lexbuf
 else backtrack lexbuf
 end)
and Tok lexbuf =
  (setLexLastAction lexbuf (magic dummyAction);
   setLexStartPos lexbuf (getLexCurrPos lexbuf);
   state_0 lexbuf)

(* The following checks type consistency of actions *)
val _ = fn _ => [action_25, action_24, action_23, action_22, action_21, action_20, action_19, action_18, action_17, action_16, action_15, action_14, action_13, action_12, action_11, action_10, action_9, action_8, action_7, action_6, action_5, action_4, action_3, action_2, action_1, action_0];

end
