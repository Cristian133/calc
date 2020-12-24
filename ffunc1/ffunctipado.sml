open ffunctipo
open ffuncast
open ffuncemit

fun tipaExp(Nro n) _ = (TInt, emitNro n)
| tipaExp(Texto s) _ = (TString, emitTexto s)
| tipaExp Unit _ = (TUnit, emitUnit())
| tipaExp(Id s) env =
	(case ffuncenv.peek env s of
	NONE => raise Fail("var "^s^" inexistente")
	| SOME(Var(v, t)) => (t, emitId v false)
	| SOME(Func{func, arg, tipo, ...}) =>
		(TF(#2 arg, tipo), emitId func true))
| tipaExp(Tupla le) env =
	let	val (lt, le') = ListPair.unzip(List.map (fn e => tipaExp e env) le)
	in	(TT lt, emitTupla le') end
| tipaExp(Oper(oper , e1, e2)) env =
	let	val (t1, e1') = tipaExp e1 env
		val (t2, e2') = tipaExp e2 env
	in
		if t1=TInt andalso t2=TInt then (TInt, emitAri(oper, e1', e2'))
		else raise Fail("tipos en suma")
	end
| tipaExp(App(e, parm)) env =
	let	val (tp, ep) = tipaExp parm env
		val  texp =
				case tipaExp e env of
				(x as (TF(t1, t2), ee)) =>
					if t1=tp then (t2, emitApp ee ep)
					else raise Fail "tipos incorrectos"
				| _ => raise Fail "no es funcion"
	in	texp end
| tipaExp(e as (Call(s, parm))) env =
	(case ffuncenv.peek env s of
	SOME(Func{func, arg, tipo, ...}) =>
		let
			val texp = tipaExp parm env 
			val texp' = #1 texp
		in
			if texp'= #2 arg then (tipo, emitCall s (#2 texp))
			else raise Fail("tipos de args incorrectos") 
		end
	| SOME _ => raise Fail(s^" no es func")
	| NONE => raise Fail(s^" inexistente"))
| tipaExp(Sec(e1, e2)) env =
	let	val (_, e1) = tipaExp e1 env
		val (t2, e2) = tipaExp e2 env
	in	(t2, emitSeq e1 e2) end
| tipaExp(Asign(s, e)) env =
	let	val (t, e') = tipaExp e env
		val tv = case ffuncenv.peek env s of
				SOME(Var(_, t)) => t
				| NONE => raise Fail(s^" inexistente")
				| _ => raise Fail(s^" no es var")
	in
		if t=tv then (tv, emitAsign s e') else raise Fail "tipos en asign"
	end
| tipaExp(If(e1, e2, e3)) env =
	let	val (t1, e1) = tipaExp e1 env
		val (t2, e2) = tipaExp e2 env
		val (t3, e3) = tipaExp e3 env
	in
		if t1<>TInt then raise Fail "test de if"
		else if t2<>t3 then raise Fail "tipos de if distintos"
		else (t2, emitIf e1 e2 e3)
	end
| tipaExp(Cmp(oper, e1, e2)) env =
	let	val (t1, e1') = tipaExp e1 env
		val (t2, e2') = tipaExp e2 env
	in
		if t1=TUnit orelse t2=TUnit then raise Fail "unit en cmp"
		else if t1<>t2 then raise Fail "tipos distintos en cmp"
		else (TInt, emitCmp oper e1' e2') 
	end
| tipaExp(Print e) env =
	let	val (t, e') = tipaExp e env
	in
		if t=TUnit then raise Fail "print unit" else (TUnit, emitPrint e' t)
	end
| tipaExp(Cr) _ = (TUnit, emitCr())
| tipaExp(Read s) env =
	(case ffuncenv.peek ffuncenv.env s of
	SOME(Var(_, t)) => (TUnit, emitRead s t)
	| _ => raise Fail "read a no var")
| tipaExp(Lambda(s, ts, e, te)) env =
	let
		val env' = ffuncenv.insert env (s, Var(s, ts))
		val (te', ee) = tipaExp e env'
	in
		if te'=te then (TF(ts, te), emiteLambda s ee)
		else raise Fail "tipos incorrectos"
	end
| tipaExp(Let(decs, e)) env =
	let	val (env', lxs) = tipaDecs decs env []
		val (t, e') = tipaExp e env'
	in
		(t, lxs^emitLet e')
	end
| tipaExp Nop _ = (TUnit, "")
and tipaDecs [] env lexps = (env, List.foldr op^ "" (rev lexps))
| tipaDecs(Func{func, arg, body, tipo}::t) env lexps =
	let val env' = ffuncenv.insert env (#1 arg, Var(#1 arg, #2 arg))
		val (t', e') = tipaExp body env'
		val e'' = emitFunc func arg e'
	in
		if t'<>tipo then raise Fail("tipo de retorno")
		else tipaDecs t (env) (e''::lexps)
	end
| tipaDecs((v as Var(s, _))::t) env lexps = tipaDecs t (ffuncenv.insert env (s, v)) lexps
