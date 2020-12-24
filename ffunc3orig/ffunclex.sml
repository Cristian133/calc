local open Obj Lexing in


open ffuncgrm
open ffunctipo

fun atoi s = (valOf o Int.fromString) s

fun action_45 lexbuf = (
 raise Fail ("["^getLexeme lexbuf^"]") )
and action_44 lexbuf = (
 NRO(atoi(getLexeme lexbuf)) )
and action_43 lexbuf = (
 ID(getLexeme lexbuf) )
and action_42 lexbuf = (
 UNIT )
and action_41 lexbuf = (
 TEXTO(Text lexbuf) )
and action_40 lexbuf = (
 IN )
and action_39 lexbuf = (
 LET )
and action_38 lexbuf = (
 READ )
and action_37 lexbuf = (
 CR )
and action_36 lexbuf = (
 PRINT )
and action_35 lexbuf = (
 ELSE )
and action_34 lexbuf = (
 THEN )
and action_33 lexbuf = (
 IF )
and action_32 lexbuf = (
 END )
and action_31 lexbuf = (
 FUN )
and action_30 lexbuf = (
 TIPO TUnit )
and action_29 lexbuf = (
 TIPO TString )
and action_28 lexbuf = (
 TIPO TInt )
and action_27 lexbuf = (
 FLECHA )
and action_26 lexbuf = (
 ASIGN )
and action_25 lexbuf = (
 DIST )
and action_24 lexbuf = (
 MAYIG )
and action_23 lexbuf = (
 MENIG )
and action_22 lexbuf = (
 MAYOR )
and action_21 lexbuf = (
 MENOR )
and action_20 lexbuf = (
 IGUAL )
and action_19 lexbuf = (
 PUNTO )
and action_18 lexbuf = (
 DOSP )
and action_17 lexbuf = (
 PCOMA )
and action_16 lexbuf = (
 COMA )
and action_15 lexbuf = (
 DIV )
and action_14 lexbuf = (
 POR )
and action_13 lexbuf = (
 MENOS )
and action_12 lexbuf = (
 MAS )
and action_11 lexbuf = (
 PD )
and action_10 lexbuf = (
 PI )
and action_9 lexbuf = (
 Coment lexbuf; Tok lexbuf )
and action_8 lexbuf = (
 Tok lexbuf )
and action_7 lexbuf = (
 EOF )
and action_6 lexbuf = (
 getLexeme lexbuf^Text lexbuf )
and action_5 lexbuf = (
 "" )
and action_4 lexbuf = (
 raise Fail "nl en string" )
and action_3 lexbuf = (
 raise Fail "string incompleta" )
and action_2 lexbuf = (
 Coment lexbuf )
and action_1 lexbuf = (
 () )
and action_0 lexbuf = (
 raise Fail "comentario incompleto" )
and state_0 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"\n" => action_1 lexbuf
 |  #"\^@" => action_0 lexbuf
 |  _ => action_2 lexbuf
 end)
and state_1 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"\"" => action_5 lexbuf
 |  #"\n" => action_4 lexbuf
 |  #"\^@" => action_3 lexbuf
 |  _ => action_6 lexbuf
 end)
and state_2 lexbuf = (
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_16 lexbuf
 else case currChar of
    #"b" => state_22 lexbuf
 |  #"a" => state_22 lexbuf
 |  #"d" => state_22 lexbuf
 |  #"h" => state_22 lexbuf
 |  #"g" => state_22 lexbuf
 |  #"k" => state_22 lexbuf
 |  #"j" => state_22 lexbuf
 |  #"o" => state_22 lexbuf
 |  #"n" => state_22 lexbuf
 |  #"m" => state_22 lexbuf
 |  #"q" => state_22 lexbuf
 |  #"z" => state_22 lexbuf
 |  #"y" => state_22 lexbuf
 |  #"x" => state_22 lexbuf
 |  #"w" => state_22 lexbuf
 |  #"v" => state_22 lexbuf
 |  #"\n" => action_8 lexbuf
 |  #"\t" => action_8 lexbuf
 |  #" " => action_8 lexbuf
 |  #"u" => state_32 lexbuf
 |  #"t" => state_31 lexbuf
 |  #"s" => state_30 lexbuf
 |  #"r" => state_29 lexbuf
 |  #"p" => state_28 lexbuf
 |  #"l" => state_27 lexbuf
 |  #"i" => state_26 lexbuf
 |  #"f" => state_25 lexbuf
 |  #"e" => state_24 lexbuf
 |  #"c" => state_23 lexbuf
 |  #">" => state_21 lexbuf
 |  #"=" => action_20 lexbuf
 |  #"<" => state_19 lexbuf
 |  #";" => action_17 lexbuf
 |  #":" => state_17 lexbuf
 |  #"/" => action_15 lexbuf
 |  #"." => action_19 lexbuf
 |  #"-" => state_13 lexbuf
 |  #"," => action_16 lexbuf
 |  #"+" => action_12 lexbuf
 |  #"*" => action_14 lexbuf
 |  #")" => action_11 lexbuf
 |  #"(" => state_8 lexbuf
 |  #"\"" => action_41 lexbuf
 |  #"!" => action_9 lexbuf
 |  #"\^@" => action_7 lexbuf
 |  _ => action_45 lexbuf
 end)
and state_8 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_10);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #")" => action_42 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_13 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_13);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #">" => action_27 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_16 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_44);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_66 lexbuf
 else backtrack lexbuf
 end)
and state_17 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_18);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"=" => action_26 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_19 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_21);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #">" => action_25 lexbuf
 |  #"=" => action_23 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_21 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_22);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"=" => action_24 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_22 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_43);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"a" andalso currChar <= #"z" then  state_33 lexbuf
 else backtrack lexbuf
 end)
and state_23 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_43);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"a" andalso currChar <= #"q" then  state_33 lexbuf
 else if currChar >= #"s" andalso currChar <= #"z" then  state_33 lexbuf
 else case currChar of
    #"r" => state_61 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_24 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_43);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"a" andalso currChar <= #"k" then  state_33 lexbuf
 else if currChar >= #"o" andalso currChar <= #"z" then  state_33 lexbuf
 else case currChar of
    #"m" => state_33 lexbuf
 |  #"n" => state_57 lexbuf
 |  #"l" => state_56 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_25 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_43);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"a" andalso currChar <= #"t" then  state_33 lexbuf
 else case currChar of
    #"z" => state_33 lexbuf
 |  #"y" => state_33 lexbuf
 |  #"x" => state_33 lexbuf
 |  #"w" => state_33 lexbuf
 |  #"v" => state_33 lexbuf
 |  #"u" => state_54 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_26 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_43);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"g" andalso currChar <= #"m" then  state_33 lexbuf
 else if currChar >= #"o" andalso currChar <= #"z" then  state_33 lexbuf
 else case currChar of
    #"e" => state_33 lexbuf
 |  #"d" => state_33 lexbuf
 |  #"c" => state_33 lexbuf
 |  #"b" => state_33 lexbuf
 |  #"a" => state_33 lexbuf
 |  #"n" => state_52 lexbuf
 |  #"f" => state_51 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_27 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_43);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"f" andalso currChar <= #"z" then  state_33 lexbuf
 else case currChar of
    #"d" => state_33 lexbuf
 |  #"c" => state_33 lexbuf
 |  #"b" => state_33 lexbuf
 |  #"a" => state_33 lexbuf
 |  #"e" => state_49 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_28 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_43);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"a" andalso currChar <= #"q" then  state_33 lexbuf
 else if currChar >= #"s" andalso currChar <= #"z" then  state_33 lexbuf
 else case currChar of
    #"r" => state_45 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_29 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_43);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"f" andalso currChar <= #"z" then  state_33 lexbuf
 else case currChar of
    #"d" => state_33 lexbuf
 |  #"c" => state_33 lexbuf
 |  #"b" => state_33 lexbuf
 |  #"a" => state_33 lexbuf
 |  #"e" => state_42 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_30 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_43);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"a" andalso currChar <= #"s" then  state_33 lexbuf
 else if currChar >= #"u" andalso currChar <= #"z" then  state_33 lexbuf
 else case currChar of
    #"t" => state_40 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_31 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_43);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"a" andalso currChar <= #"g" then  state_33 lexbuf
 else if currChar >= #"i" andalso currChar <= #"z" then  state_33 lexbuf
 else case currChar of
    #"h" => state_37 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_32 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_43);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"a" andalso currChar <= #"m" then  state_33 lexbuf
 else if currChar >= #"o" andalso currChar <= #"z" then  state_33 lexbuf
 else case currChar of
    #"n" => state_34 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_33 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_43);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"a" andalso currChar <= #"z" then  state_33 lexbuf
 else backtrack lexbuf
 end)
and state_34 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_43);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"a" andalso currChar <= #"h" then  state_33 lexbuf
 else if currChar >= #"j" andalso currChar <= #"z" then  state_33 lexbuf
 else case currChar of
    #"i" => state_35 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_35 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_43);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"a" andalso currChar <= #"s" then  state_33 lexbuf
 else if currChar >= #"u" andalso currChar <= #"z" then  state_33 lexbuf
 else case currChar of
    #"t" => state_36 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_36 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_30);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"a" andalso currChar <= #"z" then  state_33 lexbuf
 else backtrack lexbuf
 end)
and state_37 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_43);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"f" andalso currChar <= #"z" then  state_33 lexbuf
 else case currChar of
    #"d" => state_33 lexbuf
 |  #"c" => state_33 lexbuf
 |  #"b" => state_33 lexbuf
 |  #"a" => state_33 lexbuf
 |  #"e" => state_38 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_38 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_43);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"a" andalso currChar <= #"m" then  state_33 lexbuf
 else if currChar >= #"o" andalso currChar <= #"z" then  state_33 lexbuf
 else case currChar of
    #"n" => state_39 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_39 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_34);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"a" andalso currChar <= #"z" then  state_33 lexbuf
 else backtrack lexbuf
 end)
and state_40 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_43);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"a" andalso currChar <= #"q" then  state_33 lexbuf
 else if currChar >= #"s" andalso currChar <= #"z" then  state_33 lexbuf
 else case currChar of
    #"r" => state_41 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_41 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_29);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"a" andalso currChar <= #"z" then  state_33 lexbuf
 else backtrack lexbuf
 end)
and state_42 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_43);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"b" andalso currChar <= #"z" then  state_33 lexbuf
 else case currChar of
    #"a" => state_43 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_43 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_43);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"e" andalso currChar <= #"z" then  state_33 lexbuf
 else case currChar of
    #"c" => state_33 lexbuf
 |  #"b" => state_33 lexbuf
 |  #"a" => state_33 lexbuf
 |  #"d" => state_44 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_44 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_38);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"a" andalso currChar <= #"z" then  state_33 lexbuf
 else backtrack lexbuf
 end)
and state_45 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_43);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"a" andalso currChar <= #"h" then  state_33 lexbuf
 else if currChar >= #"j" andalso currChar <= #"z" then  state_33 lexbuf
 else case currChar of
    #"i" => state_46 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_46 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_43);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"a" andalso currChar <= #"m" then  state_33 lexbuf
 else if currChar >= #"o" andalso currChar <= #"z" then  state_33 lexbuf
 else case currChar of
    #"n" => state_47 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_47 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_43);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"a" andalso currChar <= #"s" then  state_33 lexbuf
 else if currChar >= #"u" andalso currChar <= #"z" then  state_33 lexbuf
 else case currChar of
    #"t" => state_48 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_48 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_36);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"a" andalso currChar <= #"z" then  state_33 lexbuf
 else backtrack lexbuf
 end)
and state_49 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_43);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"a" andalso currChar <= #"s" then  state_33 lexbuf
 else if currChar >= #"u" andalso currChar <= #"z" then  state_33 lexbuf
 else case currChar of
    #"t" => state_50 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_50 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_39);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"a" andalso currChar <= #"z" then  state_33 lexbuf
 else backtrack lexbuf
 end)
and state_51 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_33);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"a" andalso currChar <= #"z" then  state_33 lexbuf
 else backtrack lexbuf
 end)
and state_52 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_40);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"a" andalso currChar <= #"s" then  state_33 lexbuf
 else if currChar >= #"u" andalso currChar <= #"z" then  state_33 lexbuf
 else case currChar of
    #"t" => state_53 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_53 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_28);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"a" andalso currChar <= #"z" then  state_33 lexbuf
 else backtrack lexbuf
 end)
and state_54 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_43);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"a" andalso currChar <= #"m" then  state_33 lexbuf
 else if currChar >= #"o" andalso currChar <= #"z" then  state_33 lexbuf
 else case currChar of
    #"n" => state_55 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_55 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_31);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"a" andalso currChar <= #"z" then  state_33 lexbuf
 else backtrack lexbuf
 end)
and state_56 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_43);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"a" andalso currChar <= #"r" then  state_33 lexbuf
 else if currChar >= #"t" andalso currChar <= #"z" then  state_33 lexbuf
 else case currChar of
    #"s" => state_59 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_57 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_43);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"e" andalso currChar <= #"z" then  state_33 lexbuf
 else case currChar of
    #"c" => state_33 lexbuf
 |  #"b" => state_33 lexbuf
 |  #"a" => state_33 lexbuf
 |  #"d" => state_58 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_58 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_32);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"a" andalso currChar <= #"z" then  state_33 lexbuf
 else backtrack lexbuf
 end)
and state_59 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_43);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"f" andalso currChar <= #"z" then  state_33 lexbuf
 else case currChar of
    #"d" => state_33 lexbuf
 |  #"c" => state_33 lexbuf
 |  #"b" => state_33 lexbuf
 |  #"a" => state_33 lexbuf
 |  #"e" => state_60 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_60 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_35);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"a" andalso currChar <= #"z" then  state_33 lexbuf
 else backtrack lexbuf
 end)
and state_61 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_37);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"a" andalso currChar <= #"z" then  state_33 lexbuf
 else backtrack lexbuf
 end)
and state_66 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_44);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_66 lexbuf
 else backtrack lexbuf
 end)
and Tok lexbuf =
  (setLexLastAction lexbuf (magic dummyAction);
   setLexStartPos lexbuf (getLexCurrPos lexbuf);
   state_2 lexbuf)

and Text lexbuf =
  (setLexLastAction lexbuf (magic dummyAction);
   setLexStartPos lexbuf (getLexCurrPos lexbuf);
   state_1 lexbuf)

and Coment lexbuf =
  (setLexLastAction lexbuf (magic dummyAction);
   setLexStartPos lexbuf (getLexCurrPos lexbuf);
   state_0 lexbuf)

(* The following checks type consistency of actions *)
val _ = fn _ => [action_45, action_44, action_43, action_42, action_41, action_40, action_39, action_38, action_37, action_36, action_35, action_34, action_33, action_32, action_31, action_30, action_29, action_28, action_27, action_26, action_25, action_24, action_23, action_22, action_21, action_20, action_19, action_18, action_17, action_16, action_15, action_14, action_13, action_12, action_11, action_10, action_9, action_8, action_7];
val _ = fn _ => [action_6, action_5, action_4, action_3];
val _ = fn _ => [action_2, action_1, action_0];

end
