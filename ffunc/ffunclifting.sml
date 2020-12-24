open ffuncast

local
	val n = ref 0
in
	fun newName() = "__f"^Int.toString(!n)
					before n := !n+1
end
val lambdaList = ref []
val liftedLambdas = ref []
val liftedVars = ref []

fun liftExp(e as (Nro _)) = e
| liftExp(e as (Id _)) = e
| liftExp Nop = Nop
| liftExp(Suma(e1, e2)) = Suma(liftExp e1, liftExp e2)
| liftExp(Resta(e1, e2)) = Resta(liftExp e1, liftExp e2)
| liftExp(Prod(e1, e2)) = Prod(liftExp e1, liftExp e2)
| liftExp(Div(e1, e2)) = Div(liftExp e1, liftExp e2)
| liftExp(App(e1, e2)) = App(liftExp e1, liftExp e2)
| liftExp(Call(s , elist)) = Call(s, List.map liftExp elist)
| liftExp(Sec(e1, e2)) = Sec(liftExp e1, liftExp e2)
| liftExp(Asign(s , e)) = Asign(s, liftExp e)
| liftExp(If(e1, e2, e3)) = If(liftExp e1, liftExp e2, liftExp e3)
| liftExp(Cmp(Comp , e1 , e2)) = Cmp(Comp, liftExp e1, liftExp e2)
| liftExp(Print e) = Print(liftExp e)
| liftExp(Lambda(s , e)) =
	let	val _ = liftedVars := s:: !liftedVars
		val e' = liftExp e
		val func = newName()
		val f = Func{func=func, args= rev(!liftedVars), body=e'}
		val _ = lambdaList := func:: !lambdaList
		val _ = liftedLambdas := f:: !liftedLambdas
		val _ = liftedVars := tl(!liftedVars)
	in
		Polyhash.insert ffuncenv.env (func, length(!liftedVars));
		Id func
	end
fun liftDec(Func{func, args, body}) = Func{func=func, args=args, body=liftExp body}
fun liftDecs decs = List.map liftDec decs
