open ffuncast

local
	val n = ref 0
in
	fun newName() = "___f"^Int.toString(!n)
					before n := !n+1
end

val lambdaList = ref []
val liftedLambdas = ref []
val liftedVars = ref []

fun liftExp env (e as (Nro _)) = (env, e)
| liftExp env (e as (Texto _)) = (env, e)
| liftExp env (e as Unit) = (env, e)
| liftExp env (e as (Id _)) = (env, e)
| liftExp env (Tupla le) =
	let val (env', le') =
					List.foldr(fn(x, (y, z)) =>
						let val (ev, ex) = liftExp y x in (ev, ex::z) end)
					(env, []) le
	in	(env', Tupla le') end
| liftExp env Nop = (env, Nop)
| liftExp env (Oper(oper, e1, e2)) =
	let	val (env', e1') = liftExp env e1
		val (env'', e2') = liftExp env' e2
	in	(env'', Oper(oper, e1', e2')) end
| liftExp env (App(e1, e2)) = 
	let	val (env', e1') = liftExp env e1
		val (env'', e2') = liftExp env' e2
	in	(env'', App(e1', e2')) end
| liftExp env (Call(s , e)) = 
	let val (env', e') = liftExp env e 
	in	(env', Call(s, e')) end
| liftExp env (Sec(e1, e2)) = 
	let	val (env', e1') = liftExp env e1
		val (env'', e2') = liftExp env' e2
	in	(env'', Sec(e1', e2')) end
| liftExp env (Asign(s , e)) =
	let	val (env', e') = liftExp env e
	in	(env', Asign(s, e')) end
| liftExp env (If(e1, e2, e3)) =
	let	val (env', e1') = liftExp env e1
		val (env'', e2') = liftExp env' e2
		val (env''', e3') = liftExp env'' e3
	in	(env''', If(e1', e2', e3')) end
| liftExp env (Cmp(Comp , e1 , e2)) =
	let	val (env', e1') = liftExp env e1
		val (env'', e2') = liftExp env' e2
	in	(env'', Cmp(Comp, e1', e2')) end
| liftExp env (Print e) =
	let	val (env', e') = liftExp env e
	in	(env', Print e') end
| liftExp env Cr = (env, Cr)
| liftExp env (r as (Read _)) = (env, r)
(* Esta Lambda tiene todas las variables en el camino
| liftExp env (Lambda(s, ts , e, te)) =
	let	val _ = liftedVars := (s, ts):: !liftedVars
		val (env', e') = liftExp env e
		val func = newName()
		val f = Func{func=func, arg=rev(!liftedVars), body=e', tipo=te}
		val _ = lambdaList := func:: !lambdaList
		val _ = liftedLambdas := f:: !liftedLambdas
		val _ = liftedVars := tl(!liftedVars)
		val env'' = ffuncenv.insert env' (func, Func{func=func,
					args=[(s, ts)], body=e', tipo=te});
	in	(env'', Id func) end
*)
| liftExp env (Lambda(s, ts , e, te)) =
	let	val _ = liftedVars := (s, ts):: !liftedVars
		val (env', e') = liftExp env e
		val func = newName()
		val f = Func{func=func, arg=(s, ts), body=e', tipo=te}
		val _ = lambdaList := func:: !lambdaList
		val _ = liftedLambdas := f:: !liftedLambdas
		val _ = liftedVars := tl(!liftedVars)
		val env'' = ffuncenv.insert env' (func, Func{func=func,
					arg=(s, ts), body=e', tipo=te});
	in	(env'', Id func) end
| liftExp env (Let(decs, exp)) =
	let	val (env', decs') = liftDecs env decs
		val (env'', exp') = liftExp env' exp
	in	(env'', Let(decs', exp')) end
and liftDec env (Func{func, arg, body, tipo}) =
	let	val (env', body') = liftExp env body
	in	(env', Func{func=func, arg=arg, body=body', tipo=tipo}) end
| liftDec env v = (env, v)
and liftDecs env decs =
	List.foldl(fn(x, (y, z)) =>
		let	val (ev, dc) = liftDec y x in	(ev, dc::z) end)
		(env, []) decs
