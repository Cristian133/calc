local open Obj Lexing in


open ffuncgrm

fun atoi s = (valOf o Int.fromString) s

fun action_29 lexbuf = (
 raise Fail ("["^getLexeme lexbuf^"]") )
and action_28 lexbuf = (
 NRO(atoi(getLexeme lexbuf)) )
and action_27 lexbuf = (
 ID(getLexeme lexbuf) )
and action_26 lexbuf = (
 PRINT )
and action_25 lexbuf = (
 ELSE )
and action_24 lexbuf = (
 THEN )
and action_23 lexbuf = (
 IF )
and action_22 lexbuf = (
 END )
and action_21 lexbuf = (
 FN )
and action_20 lexbuf = (
 FUN )
and action_19 lexbuf = (
 FLECHA )
and action_18 lexbuf = (
 ASIGN )
and action_17 lexbuf = (
 DIST )
and action_16 lexbuf = (
 MAYIG )
and action_15 lexbuf = (
 MENIG )
and action_14 lexbuf = (
 MAYOR )
and action_13 lexbuf = (
 MENOR )
and action_12 lexbuf = (
 IGUAL )
and action_11 lexbuf = (
 DOSP )
and action_10 lexbuf = (
 PCOMA )
and action_9 lexbuf = (
 COMA )
and action_8 lexbuf = (
 ARRO )
and action_7 lexbuf = (
 DIV )
and action_6 lexbuf = (
 POR )
and action_5 lexbuf = (
 MENOS )
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
 if currChar >= #"j" andalso currChar <= #"o" then  state_18 lexbuf
 else if currChar >= #"u" andalso currChar <= #"z" then  state_18 lexbuf
 else if currChar >= #"0" andalso currChar <= #"9" then  state_11 lexbuf
 else case currChar of
    #"d" => state_18 lexbuf
 |  #"c" => state_18 lexbuf
 |  #"b" => state_18 lexbuf
 |  #"a" => state_18 lexbuf
 |  #"h" => state_18 lexbuf
 |  #"g" => state_18 lexbuf
 |  #"s" => state_18 lexbuf
 |  #"r" => state_18 lexbuf
 |  #"q" => state_18 lexbuf
 |  #"\n" => action_1 lexbuf
 |  #"\t" => action_1 lexbuf
 |  #" " => action_1 lexbuf
 |  #"t" => state_23 lexbuf
 |  #"p" => state_22 lexbuf
 |  #"i" => state_21 lexbuf
 |  #"f" => state_20 lexbuf
 |  #"e" => state_19 lexbuf
 |  #"@" => action_8 lexbuf
 |  #">" => state_16 lexbuf
 |  #"=" => action_12 lexbuf
 |  #"<" => state_14 lexbuf
 |  #";" => action_10 lexbuf
 |  #":" => state_12 lexbuf
 |  #"/" => action_7 lexbuf
 |  #"-" => state_9 lexbuf
 |  #"," => action_9 lexbuf
 |  #"+" => action_4 lexbuf
 |  #"*" => action_6 lexbuf
 |  #")" => action_3 lexbuf
 |  #"(" => action_2 lexbuf
 |  #"\^@" => action_0 lexbuf
 |  _ => action_29 lexbuf
 end)
and state_9 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_5);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #">" => action_19 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_11 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_28);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_45 lexbuf
 else backtrack lexbuf
 end)
and state_12 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_11);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"=" => action_18 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_14 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_13);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #">" => action_17 lexbuf
 |  #"=" => action_15 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_16 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_14);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"=" => action_16 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_18 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_27);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"a" andalso currChar <= #"z" then  state_24 lexbuf
 else backtrack lexbuf
 end)
and state_19 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_27);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"a" andalso currChar <= #"k" then  state_24 lexbuf
 else if currChar >= #"o" andalso currChar <= #"z" then  state_24 lexbuf
 else case currChar of
    #"m" => state_24 lexbuf
 |  #"n" => state_37 lexbuf
 |  #"l" => state_36 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_20 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_27);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"a" andalso currChar <= #"m" then  state_24 lexbuf
 else if currChar >= #"o" andalso currChar <= #"t" then  state_24 lexbuf
 else case currChar of
    #"z" => state_24 lexbuf
 |  #"y" => state_24 lexbuf
 |  #"x" => state_24 lexbuf
 |  #"w" => state_24 lexbuf
 |  #"v" => state_24 lexbuf
 |  #"u" => state_34 lexbuf
 |  #"n" => state_33 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_21 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_27);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"g" andalso currChar <= #"z" then  state_24 lexbuf
 else case currChar of
    #"e" => state_24 lexbuf
 |  #"d" => state_24 lexbuf
 |  #"c" => state_24 lexbuf
 |  #"b" => state_24 lexbuf
 |  #"a" => state_24 lexbuf
 |  #"f" => state_32 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_22 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_27);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"a" andalso currChar <= #"q" then  state_24 lexbuf
 else if currChar >= #"s" andalso currChar <= #"z" then  state_24 lexbuf
 else case currChar of
    #"r" => state_28 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_23 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_27);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"a" andalso currChar <= #"g" then  state_24 lexbuf
 else if currChar >= #"i" andalso currChar <= #"z" then  state_24 lexbuf
 else case currChar of
    #"h" => state_25 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_24 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_27);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"a" andalso currChar <= #"z" then  state_24 lexbuf
 else backtrack lexbuf
 end)
and state_25 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_27);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"f" andalso currChar <= #"z" then  state_24 lexbuf
 else case currChar of
    #"d" => state_24 lexbuf
 |  #"c" => state_24 lexbuf
 |  #"b" => state_24 lexbuf
 |  #"a" => state_24 lexbuf
 |  #"e" => state_26 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_26 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_27);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"a" andalso currChar <= #"m" then  state_24 lexbuf
 else if currChar >= #"o" andalso currChar <= #"z" then  state_24 lexbuf
 else case currChar of
    #"n" => state_27 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_27 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_24);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"a" andalso currChar <= #"z" then  state_24 lexbuf
 else backtrack lexbuf
 end)
and state_28 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_27);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"a" andalso currChar <= #"h" then  state_24 lexbuf
 else if currChar >= #"j" andalso currChar <= #"z" then  state_24 lexbuf
 else case currChar of
    #"i" => state_29 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_29 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_27);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"a" andalso currChar <= #"m" then  state_24 lexbuf
 else if currChar >= #"o" andalso currChar <= #"z" then  state_24 lexbuf
 else case currChar of
    #"n" => state_30 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_30 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_27);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"a" andalso currChar <= #"s" then  state_24 lexbuf
 else if currChar >= #"u" andalso currChar <= #"z" then  state_24 lexbuf
 else case currChar of
    #"t" => state_31 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_31 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_26);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"a" andalso currChar <= #"z" then  state_24 lexbuf
 else backtrack lexbuf
 end)
and state_32 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_23);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"a" andalso currChar <= #"z" then  state_24 lexbuf
 else backtrack lexbuf
 end)
and state_33 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_21);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"a" andalso currChar <= #"z" then  state_24 lexbuf
 else backtrack lexbuf
 end)
and state_34 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_27);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"a" andalso currChar <= #"m" then  state_24 lexbuf
 else if currChar >= #"o" andalso currChar <= #"z" then  state_24 lexbuf
 else case currChar of
    #"n" => state_35 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_35 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_20);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"a" andalso currChar <= #"z" then  state_24 lexbuf
 else backtrack lexbuf
 end)
and state_36 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_27);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"a" andalso currChar <= #"r" then  state_24 lexbuf
 else if currChar >= #"t" andalso currChar <= #"z" then  state_24 lexbuf
 else case currChar of
    #"s" => state_39 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_37 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_27);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"e" andalso currChar <= #"z" then  state_24 lexbuf
 else case currChar of
    #"c" => state_24 lexbuf
 |  #"b" => state_24 lexbuf
 |  #"a" => state_24 lexbuf
 |  #"d" => state_38 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_38 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_22);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"a" andalso currChar <= #"z" then  state_24 lexbuf
 else backtrack lexbuf
 end)
and state_39 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_27);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"f" andalso currChar <= #"z" then  state_24 lexbuf
 else case currChar of
    #"d" => state_24 lexbuf
 |  #"c" => state_24 lexbuf
 |  #"b" => state_24 lexbuf
 |  #"a" => state_24 lexbuf
 |  #"e" => state_40 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_40 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_25);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"a" andalso currChar <= #"z" then  state_24 lexbuf
 else backtrack lexbuf
 end)
and state_45 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_28);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_45 lexbuf
 else backtrack lexbuf
 end)
and Tok lexbuf =
  (setLexLastAction lexbuf (magic dummyAction);
   setLexStartPos lexbuf (getLexCurrPos lexbuf);
   state_0 lexbuf)

(* The following checks type consistency of actions *)
val _ = fn _ => [action_29, action_28, action_27, action_26, action_25, action_24, action_23, action_22, action_21, action_20, action_19, action_18, action_17, action_16, action_15, action_14, action_13, action_12, action_11, action_10, action_9, action_8, action_7, action_6, action_5, action_4, action_3, action_2, action_1, action_0];

end
