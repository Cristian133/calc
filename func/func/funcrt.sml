val prol = 
	"create _input-buffer 80 chars allot\n\
	\: getint\n\
	\begin _input-buffer 80 accept\n\
	\_input-buffer swap\n\
	\s>number?  invert\n\
	\while\n\
	\.\" Ingrese nuevamente\" cr\n\
	\2drop\n\
	\repeat drop ;\n"

