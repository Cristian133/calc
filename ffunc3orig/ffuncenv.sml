open ffuncast
open ffunctipo

fun newenv() = Polyhash.mkPolyTable(50, Empty)
fun insert env s =
	let	val env' = Polyhash.copy env
	in	Polyhash.insert env' s; env' end
fun peek env s = Polyhash.peek env s

val defers = ref [] : string list ref
val env = Polyhash.mkPolyTable(50, Empty)
	: (string, Decl) Polyhash.hash_table

fun creaEnv[] env = env
| creaEnv(h::t) env =
	creaEnv t (case h of
		(fd as (Func{f, ...})) =>
			insert env (f, fd)
		| (vd as (Var{v, ...})) =>
			insert env (v, vd))
fun gendefers () =
	List.foldr(fn(f, t) => "defer _"^f^"\n"^t) "" (!defers)
