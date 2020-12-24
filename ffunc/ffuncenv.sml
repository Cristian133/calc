open ffuncast

val env = Polyhash.mkPolyTable(50, Empty)

fun creaEnv[] = ()
| creaEnv(Func{func, args, ...}::t) =
	(Polyhash.insert env (func, length args); creaEnv t)
