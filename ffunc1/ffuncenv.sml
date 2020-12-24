open ffuncast
open ffunctipo

fun newenv() = Polyhash.mkPolyTable(50, Empty)
fun insert env s =
	let	val env' = Polyhash.copy env
	in	Polyhash.insert env' s; env' end
fun peek env s = Polyhash.peek env s

val env = Polyhash.mkPolyTable(50, Empty)
	: (string, Decl) Polyhash.hash_table

fun creaEnv[] env = env
| creaEnv(h::t) env =
	creaEnv t (case h of
		(fd as (Func{func, ...})) =>
			insert env (func, fd)
		| (vd as (Var(s, _))) =>
			insert env (s, vd))
fun gendefers env =
	let	fun gen [] defs vars = (defs, vars)
		| gen((Func{func, ...})::t) defs vars =
			gen t (defs^"defer "^func^"\n") vars
		| gen((Var(s, _))::t) defs vars =
			gen t defs (vars^"variable "^s^"\n")
	in	gen (List.map (#2) (Polyhash.listItems env)) "" "" end
