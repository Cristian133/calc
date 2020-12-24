open tfunctipo
open tfuncast
open tfuncemit

fun tipaExp(Nro n) env = (Int, emitNro n)
| tipaExp(Texto s) env = (String, emitTexto s)
| tipaExp(Id s) env =
	(case tfuncenv.peek env s of
	NONE => raise Fail("var "^s^" inexistente")
	| SOME(Var(v, t)) => (t, emitId v)
	| SOME _ => raise Fail(s^" es func"))
| tipaExp(Ari(oper , e1, e2)) env =
	let	val (t1, e1') = tipaExp e1 env
		val (t2, e2') = tipaExp e2 env
	in
		if t1=Int andalso t2=Int then (Int, emitAri(oper, e1', e2'))
		else raise Fail("tipos en suma")
	end
| tipaExp(e as (Call(s, parms))) env =
	(case tfuncenv.peek env s of
	SOME(Func{f, args, res, ...}) =>
		let	val texp = List.map (fn x => tipaExp x env) parms
			val texp' = List.map (#1) texp
		in
			(if List.all op=
				(ListPair.zipEq(texp',
					List.map (#2) args)) then (res, emitCall s (List.map (#2) texp))
			else raise Fail("tipos de args incorrectos")) 
			handle UnequalLengths => raise Fail("nro de args")
		end
	| SOME _ => raise Fail(s^" no es func")
	| NONE => raise Fail(s^" inexistente"))
| tipaExp(Sec(e1, e2)) env =
	let	val (_, e1) = tipaExp e1 env
		val (t2, e2) = tipaExp e2 env
	in	(t2, emitSeq e1 e2) end
| tipaExp(Asign(s, e)) env =
	let	val (t, e') = tipaExp e env
		val tv = case tfuncenv.peek env s of
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
		if t1<>Int then raise Fail "test de if"
		else if t2<>t3 then raise Fail "tipos de if distintos"
		else (t2, emitIf e1 e2 e3)
	end
| tipaExp(Cmp(oper, e1, e2)) env =
	let	val (t1, e1') = tipaExp e1 env
		val (t2, e2') = tipaExp e2 env
	in
		if t1=Unit orelse t2=Unit then raise Fail "unit en cmp"
		else if t1<>t2 then raise Fail "tipos distintos en cmp"
		else (Int, emitCmp oper e1' e2') 
	end
| tipaExp(Print e) env =
	let	val (t, e') = tipaExp e env
	in
		if t=Unit then raise Fail "print unit" else (Unit, emitPrint e' t)
	end
| tipaExp(Cr) _ = (Unit, emitCr())
| tipaExp(Read e) env =
	(case e of
	Id _ =>
		let	val (t, e') = tipaExp e env
		in
			(Unit, emitRead e' t)
		end
	| _ => raise Fail "error read")
| tipaExp(Let(decs, e)) env =
	let	val (env', lxs) = tipaDecs decs env []
		val (t, e') = tipaExp e env'
	in
		(t, lxs^emitLet e')
	end
and tipaDecs [] env lexps = (env, List.foldr op^ "" (rev lexps))
| tipaDecs(Func{f, args, res, body}::t) env lexps =
	let val env' = List.foldr(fn((s, tt), y) => tfuncenv.insert y (s, Var(s, tt))) env args
		val (t', e') = tipaExp body env'
		val e'' = emitFunc f args e'
	in
		if t'<>res then raise Fail("tipo de retorno")
		else tipaDecs t (env) (e''::lexps)
	end
| tipaDecs((v as Var(s, _))::t) env lexps = tipaDecs t (tfuncenv.insert env (s, v)) lexps
