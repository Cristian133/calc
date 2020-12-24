fun despliega (Tiint::t) cnt prof = 
	roll1 prof^" over "^tS cnt^" cells + !"^ despliega t (cnt+1) prof

|	despliega (TString::t) cnt prof = 
	roll2 (prof+1)^" over "^tS cnt^" cells + ! "^ despliega t (cnt+1) prof

	despliega ((TT lt)::t) cnt prof = 
	"here tS "^(length lt)^" cells allot \n"^ despliega lt 0 (prof+1)^" swap dup rot "tS cnt^"cells + !\n" despliega t (cnt+1) prof

|	despliega []_ _ = ""

fun roll1 0 = ""
|	roll1 n = tS n^" roll"
fun roll2 0 = ""
|	roll2 1 = " swap _mkstring "
|	roll2 2 = " rot _mkstring "
|	roll2 n = tS n ^" roll "^tS n^" roll _mkstring "