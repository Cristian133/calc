val prologo=
	"create _input-buffer 80 allot\n\
	\: getint\n\
	\begin _input-buffer 80 accept \n\
	\_input-buffer swap s>number?  invert\n\
	\while \n\
	\	.\" Ingrese nuevamente\" cr\n\
	\	2drop\n\
	\repeat drop\n"s