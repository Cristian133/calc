open ffunctipo
open ffuncast
open ffuncemit

local
	val rn = ref 0
	val stack = ref [] : string list ref
in
	fun nnombre f = f^"."^Int.toString(!rn) before rn := !rn+1
	fun push f = stack := f::(!stack)
	fun pop() = hd(!stack) before stack := tl(!stack)
	fun vacia() = null(!stack)
end

fun procFunc nombre ff arg tipoa body tipob env prf =
	let val env' = ffuncenv.insert env (arg, Var{v=arg, t=tipoa, pr=prf})
		val (t', e', p) = tipaExp body env' prf
		val e'' = emitFunc nombre (arg, tipoa) tipob e'
	in
		if t'<>tipob then raise Fail("tipo de retorno")
		else (e'', prf)
	end
and tipaExp(Nro n) _ prf = (TInt, emitNro n, prf)
| tipaExp(Texto s) _ prf = (TString, emitTexto s, prf)
| tipaExp Unit _ prf = (TUnit, emitUnit(), prf)
| tipaExp(Id s) env prf =
	(case ffuncenv.peek env s of
	NONE => raise Fail("var "^s^" inexistente")
	| SOME(Var{v, t, pr}) => (t, emitId v t (pr-prf) false, pr)
	| SOME(Func{f, arg, tipo, pr, ...}) =>
		(TF(#2 arg, tipo), emitId f tipo 0 true, pr))
| tipaExp(Tupla le) env prf =
	let	fun unzip3 l =
			let	fun u [] (t1, t2, t3) = (rev t1, rev t2, rev t3)
				| u ((h1, h2, h3)::t) (t1, t2, t3) =
					u t (h1::t1, h2::t2, h3::t3)
			in	u l ([], [], []) end
		val (lt, le', lp) = unzip3(List.map (fn e => tipaExp e env prf) le)
	in	(TT lt, emitTupla lt le', prf) end
| tipaExp(Memb(e, n)) env prf =
	let	val (te, e', p) = tipaExp e env prf
	in
		case te of
		TT lt =>
			let	val te' = List.nth(lt, n)
						handle Subscript => raise Fail "indice incorrecto"
			in	(te', emitMemb lt e' n te', prf) end
		| _ => raise Fail "no es tupla"
	end
| tipaExp(Oper(oper , e1, e2)) env prf =
	let	val (t1, e1', p1) = tipaExp e1 env prf
		val (t2, e2', p2) = tipaExp e2 env prf
	in
		if t1=TInt andalso t2=TInt then (TInt, emitAri(oper, e1', e2'), prf)
		else raise Fail("tipos en suma")
	end
| tipaExp(e as (Call(s, parm))) env prf =
	let
		val tes = tipaExp s env prf
		val texp = tipaExp parm env prf
		val tes' = #2 tes
		val texp' = #1 texp
		val prf' = #3 tes
	in
		case #1 tes of
		TF(td, tc) =>
			if td = texp' then (tc, emitCall tes' (#2 texp) tc (prf'-prf), prf)
			else raise Fail "tipos de args incorrectos"
		| _ => raise Fail("1:"^ (toStr texp'))
	end
| tipaExp(Sec(e1, e2)) env prf =
	let	val (_, e1, p1) = tipaExp e1 env prf
		val (t2, e2, p2) = tipaExp e2 env prf
	in	(t2, emitSeq e1 e2, prf) end
| tipaExp(Asign(m as (Memb(t, n)), e)) env prf =
	let	val (tm, em, pm) = tipaExp m env prf
		val (t, e', pe) = tipaExp e env prf
		val em' = emitAsignTupla em tm
	in
		if tm=t then (tm, e'^" "^em', pm)
		else raise Fail "tipos en asign"
	end
| tipaExp(Asign(s, e)) env prf =
	let	val (t, e', pe) = tipaExp e env prf
		val (ts, es, ps) = tipaExp s env prf
	in
		if t=ts then (ts, emitAsign es ts (ps-prf) e', prf)
		else raise Fail "tipos en asign"
	end
| tipaExp(If(e1, e2, e3)) env prf =
	let	val (t1, e1, p1) = tipaExp e1 env prf
		val (t2, e2, p2) = tipaExp e2 env prf
		val (t3, e3, p3) = tipaExp e3 env prf
	in
		if t1<>TInt then raise Fail "test de if"
		else if t2<>t3 then raise Fail "tipos de if distintos"
		else (t2, emitIf e1 e2 e3, prf)
	end
| tipaExp(Cmp(oper, e1, e2)) env prf =
	let	val (t1, e1', p1) = tipaExp e1 env prf
		val (t2, e2', p2) = tipaExp e2 env prf
	in
		if t1=TUnit orelse t2=TUnit then raise Fail "unit en cmp"
		else if t1<>t2 then raise Fail "tipos distintos en cmp"
		else (TInt, emitCmp oper e1' e2', prf) 
	end
| tipaExp(Print e) env prf =
	let	val (t, e', p) = tipaExp e env prf
	in
		if t=TUnit then raise Fail "print unit" else (TUnit, emitPrint e' t p, prf)
	end
| tipaExp(Cr) _ prf = (TUnit, emitCr(), prf)
| tipaExp(Read s) env prf =
	(case ffuncenv.peek ffuncenv.env s of
	SOME(Var{t, pr, ...}) => (TUnit, emitRead s (pr-prf) t, prf)
	| _ => raise Fail "read a no var")
| tipaExp(Let(decs, e)) env prf =
	let	val (env', lxs) = tipaDecs decs env prf []
		val (t, e', p) = tipaExp e env' prf
	in
		(t, lxs^emitLet e', prf)
	end
| tipaExp Nop _ prf = (TUnit, "", prf)
and tipaDecs [] env _ lexps = (env, "")
| tipaDecs((Func{f, arg, body, tipo, pr})::t) env prf lexps =
	let
		val nf = nnombre f
		val ff = Func{f=nf, arg=arg, body=body, tipo=tipo, pr=prf+1}
		val env' = ffuncenv.insert env (f, ff)
		val _ = ffuncenv.defers:= nf:: !ffuncenv.defers
		val (e', p) = procFunc nf ff (#1 arg) (#2 arg) body tipo env' (prf+1)
		val _ = push e'
	in
		tipaDecs t (env') prf (e'::lexps)
	end
| tipaDecs((vv as (Var{v, pr, ...}))::t) env prf lexps =
	tipaDecs t (ffuncenv.insert env (v, vv)) prf lexps
