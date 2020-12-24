local open Obj Lexing in


open ffuncgrm
open ffunctipo

fun atoi s = (valOf o Int.fromString) s

fun action_46 lexbuf = (
 raise Fail ("["^getLexeme lexbuf^"]") )
and action_45 lexbuf = (
 NRO(atoi(getLexeme lexbuf)) )
and action_44 lexbuf = (
 ID(getLexeme lexbuf) )
and action_43 lexbuf = (
 UNIT )
and action_42 lexbuf = (
 TEXTO(Text lexbuf) )
and action_41 lexbuf = (
 ARROBA )
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
 if currChar >= #"m" andalso currChar <= #"r" then  state_24 lexbuf
 else if currChar >= #"0" andalso currChar <= #"9" then  state_16 lexbuf
 else case currChar of
    #"b" => state_24 lexbuf
 |  #"d" => state_24 lexbuf
 |  #"g" => state_24 lexbuf
 |  #"k" => state_24 lexbuf
 |  #"j" => state_24 lexbuf
 |  #"t" => state_24 lexbuf
 |  #"z" => state_24 lexbuf
 |  #"y" => state_24 lexbuf
 |  #"x" => state_24 lexbuf
 |  #"w" => state_24 lexbuf
 |  #"v" => state_24 lexbuf
 |  #"\n" => action_8 lexbuf
 |  #"\t" => action_8 lexbuf
 |  #" " => action_8 lexbuf
 |  #"u" => state_32 lexbuf
 |  #"s" => state_31 lexbuf
 |  #"l" => state_30 lexbuf
 |  #"i" => state_29 lexbuf
 |  #"h" => state_28 lexbuf
 |  #"f" => state_27 lexbuf
 |  #"e" => state_26 lexbuf
 |  #"c" => state_25 lexbuf
 |  #"a" => state_23 lexbuf
 |  #"@" => action_41 lexbuf
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
 |  #"\"" => action_42 lexbuf
 |  #"!" => action_9 lexbuf
 |  #"\^@" => action_7 lexbuf
 |  _ => action_46 lexbuf
 end)
and state_8 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_10);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #")" => action_43 lexbuf
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
 setLexLastAction lexbuf (magic action_45);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_89 lexbuf
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
and state_23 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_44);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"e" andalso currChar <= #"z" then  state_33 lexbuf
 else case currChar of
    #"c" => state_33 lexbuf
 |  #"b" => state_33 lexbuf
 |  #"a" => state_33 lexbuf
 |  #"d" => state_79 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_24 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_44);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"a" andalso currChar <= #"z" then  state_33 lexbuf
 else backtrack lexbuf
 end)
and state_25 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_44);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"b" andalso currChar <= #"z" then  state_33 lexbuf
 else case currChar of
    #"a" => state_74 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_26 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_44);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"a" andalso currChar <= #"m" then  state_33 lexbuf
 else if currChar >= #"o" andalso currChar <= #"z" then  state_33 lexbuf
 else case currChar of
    #"n" => state_64 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_27 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_44);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"a" andalso currChar <= #"h" then  state_33 lexbuf
 else if currChar >= #"j" andalso currChar <= #"t" then  state_33 lexbuf
 else case currChar of
    #"z" => state_33 lexbuf
 |  #"y" => state_33 lexbuf
 |  #"x" => state_33 lexbuf
 |  #"w" => state_33 lexbuf
 |  #"v" => state_33 lexbuf
 |  #"u" => state_57 lexbuf
 |  #"i" => state_56 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_28 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_44);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"b" andalso currChar <= #"z" then  state_33 lexbuf
 else case currChar of
    #"a" => state_52 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_29 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_44);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"a" andalso currChar <= #"l" then  state_33 lexbuf
 else if currChar >= #"n" andalso currChar <= #"z" then  state_33 lexbuf
 else case currChar of
    #"m" => state_45 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_30 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_44);
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
and state_31 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_44);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"a" andalso currChar <= #"h" then  state_33 lexbuf
 else if currChar >= #"j" andalso currChar <= #"z" then  state_33 lexbuf
 else case currChar of
    #"i" => state_39 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_32 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_44);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"a" andalso currChar <= #"m" then  state_33 lexbuf
 else if currChar >= #"o" andalso currChar <= #"z" then  state_33 lexbuf
 else case currChar of
    #"n" => state_34 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_33 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_44);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"a" andalso currChar <= #"z" then  state_33 lexbuf
 else backtrack lexbuf
 end)
and state_34 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_44);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"a" andalso currChar <= #"h" then  state_33 lexbuf
 else if currChar >= #"j" andalso currChar <= #"z" then  state_33 lexbuf
 else case currChar of
    #"i" => state_35 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_35 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_44);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"e" andalso currChar <= #"z" then  state_33 lexbuf
 else case currChar of
    #"c" => state_33 lexbuf
 |  #"b" => state_33 lexbuf
 |  #"a" => state_33 lexbuf
 |  #"d" => state_36 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_36 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_44);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"b" andalso currChar <= #"z" then  state_33 lexbuf
 else case currChar of
    #"a" => state_37 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_37 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_44);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"e" andalso currChar <= #"z" then  state_33 lexbuf
 else case currChar of
    #"c" => state_33 lexbuf
 |  #"b" => state_33 lexbuf
 |  #"a" => state_33 lexbuf
 |  #"d" => state_38 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_38 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_30);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"a" andalso currChar <= #"z" then  state_33 lexbuf
 else backtrack lexbuf
 end)
and state_39 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_33);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"a" andalso currChar <= #"m" then  state_33 lexbuf
 else if currChar >= #"o" andalso currChar <= #"z" then  state_33 lexbuf
 else case currChar of
    #"n" => state_40 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_40 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_44);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"a" andalso currChar <= #"n" then  state_33 lexbuf
 else if currChar >= #"p" andalso currChar <= #"z" then  state_33 lexbuf
 else case currChar of
    #"o" => state_41 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_41 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_35);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"a" andalso currChar <= #"z" then  state_33 lexbuf
 else backtrack lexbuf
 end)
and state_42 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_44);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"f" andalso currChar <= #"z" then  state_33 lexbuf
 else case currChar of
    #"d" => state_33 lexbuf
 |  #"c" => state_33 lexbuf
 |  #"b" => state_33 lexbuf
 |  #"a" => state_33 lexbuf
 |  #"e" => state_43 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_43 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_44);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"a" andalso currChar <= #"q" then  state_33 lexbuf
 else if currChar >= #"s" andalso currChar <= #"z" then  state_33 lexbuf
 else case currChar of
    #"r" => state_44 lexbuf
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
 setLexLastAction lexbuf (magic action_44);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"a" andalso currChar <= #"o" then  state_33 lexbuf
 else if currChar >= #"q" andalso currChar <= #"z" then  state_33 lexbuf
 else case currChar of
    #"p" => state_46 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_46 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_44);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"a" andalso currChar <= #"q" then  state_33 lexbuf
 else if currChar >= #"s" andalso currChar <= #"z" then  state_33 lexbuf
 else case currChar of
    #"r" => state_47 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_47 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_44);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"a" andalso currChar <= #"h" then  state_33 lexbuf
 else if currChar >= #"j" andalso currChar <= #"z" then  state_33 lexbuf
 else case currChar of
    #"i" => state_48 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_48 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_44);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"a" andalso currChar <= #"l" then  state_33 lexbuf
 else if currChar >= #"n" andalso currChar <= #"z" then  state_33 lexbuf
 else case currChar of
    #"m" => state_49 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_49 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_44);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"a" andalso currChar <= #"h" then  state_33 lexbuf
 else if currChar >= #"j" andalso currChar <= #"z" then  state_33 lexbuf
 else case currChar of
    #"i" => state_50 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_50 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_44);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"a" andalso currChar <= #"q" then  state_33 lexbuf
 else if currChar >= #"s" andalso currChar <= #"z" then  state_33 lexbuf
 else case currChar of
    #"r" => state_51 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_51 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_36);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"a" andalso currChar <= #"z" then  state_33 lexbuf
 else backtrack lexbuf
 end)
and state_52 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_44);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"d" andalso currChar <= #"z" then  state_33 lexbuf
 else case currChar of
    #"b" => state_33 lexbuf
 |  #"a" => state_33 lexbuf
 |  #"c" => state_53 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_53 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_44);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"f" andalso currChar <= #"z" then  state_33 lexbuf
 else case currChar of
    #"d" => state_33 lexbuf
 |  #"c" => state_33 lexbuf
 |  #"b" => state_33 lexbuf
 |  #"a" => state_33 lexbuf
 |  #"e" => state_54 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_54 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_44);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"a" andalso currChar <= #"q" then  state_33 lexbuf
 else if currChar >= #"s" andalso currChar <= #"z" then  state_33 lexbuf
 else case currChar of
    #"r" => state_55 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_55 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_39);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"a" andalso currChar <= #"z" then  state_33 lexbuf
 else backtrack lexbuf
 end)
and state_56 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_44);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"a" andalso currChar <= #"m" then  state_33 lexbuf
 else if currChar >= #"o" andalso currChar <= #"z" then  state_33 lexbuf
 else case currChar of
    #"n" => state_63 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_57 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_44);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"a" andalso currChar <= #"m" then  state_33 lexbuf
 else if currChar >= #"o" andalso currChar <= #"z" then  state_33 lexbuf
 else case currChar of
    #"n" => state_58 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_58 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_44);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"d" andalso currChar <= #"z" then  state_33 lexbuf
 else case currChar of
    #"b" => state_33 lexbuf
 |  #"a" => state_33 lexbuf
 |  #"c" => state_59 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_59 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_44);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"a" andalso currChar <= #"h" then  state_33 lexbuf
 else if currChar >= #"j" andalso currChar <= #"z" then  state_33 lexbuf
 else case currChar of
    #"i" => state_60 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_60 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_44);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"a" andalso currChar <= #"n" then  state_33 lexbuf
 else if currChar >= #"p" andalso currChar <= #"z" then  state_33 lexbuf
 else case currChar of
    #"o" => state_61 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_61 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_44);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"a" andalso currChar <= #"m" then  state_33 lexbuf
 else if currChar >= #"o" andalso currChar <= #"z" then  state_33 lexbuf
 else case currChar of
    #"n" => state_62 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_62 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_31);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"a" andalso currChar <= #"z" then  state_33 lexbuf
 else backtrack lexbuf
 end)
and state_63 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_32);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"a" andalso currChar <= #"z" then  state_33 lexbuf
 else backtrack lexbuf
 end)
and state_64 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_40);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"a" andalso currChar <= #"s" then  state_33 lexbuf
 else if currChar >= #"u" andalso currChar <= #"z" then  state_33 lexbuf
 else case currChar of
    #"t" => state_65 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_65 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_44);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"f" andalso currChar <= #"n" then  state_33 lexbuf
 else if currChar >= #"p" andalso currChar <= #"z" then  state_33 lexbuf
 else case currChar of
    #"d" => state_33 lexbuf
 |  #"c" => state_33 lexbuf
 |  #"b" => state_33 lexbuf
 |  #"a" => state_33 lexbuf
 |  #"o" => state_67 lexbuf
 |  #"e" => state_66 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_66 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_44);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"a" andalso currChar <= #"q" then  state_33 lexbuf
 else if currChar >= #"s" andalso currChar <= #"z" then  state_33 lexbuf
 else case currChar of
    #"r" => state_72 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_67 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_44);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"a" andalso currChar <= #"m" then  state_33 lexbuf
 else if currChar >= #"o" andalso currChar <= #"z" then  state_33 lexbuf
 else case currChar of
    #"n" => state_68 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_68 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_44);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"d" andalso currChar <= #"z" then  state_33 lexbuf
 else case currChar of
    #"b" => state_33 lexbuf
 |  #"a" => state_33 lexbuf
 |  #"c" => state_69 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_69 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_44);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"f" andalso currChar <= #"z" then  state_33 lexbuf
 else case currChar of
    #"d" => state_33 lexbuf
 |  #"c" => state_33 lexbuf
 |  #"b" => state_33 lexbuf
 |  #"a" => state_33 lexbuf
 |  #"e" => state_70 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_70 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_44);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"a" andalso currChar <= #"r" then  state_33 lexbuf
 else if currChar >= #"t" andalso currChar <= #"z" then  state_33 lexbuf
 else case currChar of
    #"s" => state_71 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_71 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_34);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"a" andalso currChar <= #"z" then  state_33 lexbuf
 else backtrack lexbuf
 end)
and state_72 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_44);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"a" andalso currChar <= #"n" then  state_33 lexbuf
 else if currChar >= #"p" andalso currChar <= #"z" then  state_33 lexbuf
 else case currChar of
    #"o" => state_73 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_73 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_28);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"a" andalso currChar <= #"z" then  state_33 lexbuf
 else backtrack lexbuf
 end)
and state_74 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_44);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"e" andalso currChar <= #"z" then  state_33 lexbuf
 else case currChar of
    #"c" => state_33 lexbuf
 |  #"b" => state_33 lexbuf
 |  #"a" => state_33 lexbuf
 |  #"d" => state_75 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_75 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_44);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"f" andalso currChar <= #"z" then  state_33 lexbuf
 else case currChar of
    #"d" => state_33 lexbuf
 |  #"c" => state_33 lexbuf
 |  #"b" => state_33 lexbuf
 |  #"a" => state_33 lexbuf
 |  #"e" => state_76 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_76 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_44);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"a" andalso currChar <= #"m" then  state_33 lexbuf
 else if currChar >= #"o" andalso currChar <= #"z" then  state_33 lexbuf
 else case currChar of
    #"n" => state_77 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_77 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_44);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"b" andalso currChar <= #"z" then  state_33 lexbuf
 else case currChar of
    #"a" => state_78 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_78 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_29);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"a" andalso currChar <= #"z" then  state_33 lexbuf
 else backtrack lexbuf
 end)
and state_79 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_44);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"f" andalso currChar <= #"z" then  state_33 lexbuf
 else case currChar of
    #"d" => state_33 lexbuf
 |  #"c" => state_33 lexbuf
 |  #"b" => state_33 lexbuf
 |  #"a" => state_33 lexbuf
 |  #"e" => state_80 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_80 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_44);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"a" andalso currChar <= #"m" then  state_33 lexbuf
 else if currChar >= #"o" andalso currChar <= #"z" then  state_33 lexbuf
 else case currChar of
    #"n" => state_81 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_81 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_44);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"a" andalso currChar <= #"s" then  state_33 lexbuf
 else if currChar >= #"u" andalso currChar <= #"z" then  state_33 lexbuf
 else case currChar of
    #"t" => state_82 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_82 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_44);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"a" andalso currChar <= #"q" then  state_33 lexbuf
 else if currChar >= #"s" andalso currChar <= #"z" then  state_33 lexbuf
 else case currChar of
    #"r" => state_83 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_83 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_44);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"a" andalso currChar <= #"n" then  state_33 lexbuf
 else if currChar >= #"p" andalso currChar <= #"z" then  state_33 lexbuf
 else case currChar of
    #"o" => state_84 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_84 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_37);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"a" andalso currChar <= #"z" then  state_33 lexbuf
 else backtrack lexbuf
 end)
and state_89 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_45);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_89 lexbuf
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
val _ = fn _ => [action_46, action_45, action_44, action_43, action_42, action_41, action_40, action_39, action_38, action_37, action_36, action_35, action_34, action_33, action_32, action_31, action_30, action_29, action_28, action_27, action_26, action_25, action_24, action_23, action_22, action_21, action_20, action_19, action_18, action_17, action_16, action_15, action_14, action_13, action_12, action_11, action_10, action_9, action_8, action_7];
val _ = fn _ => [action_6, action_5, action_4, action_3];
val _ = fn _ => [action_2, action_1, action_0];

end
