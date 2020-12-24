val prol = 
	"create _input-buffer 80 chars allot\n\
	\: getint\n\
	\begin _input-buffer 80 accept\n\
	\_input-buffer swap\n\
	\s>number?  invert\n\
	\while\n\
	\.\" Ingrese nuevamente\" cr\n\
	\2drop\n\
	\repeat drop ;\n\
	\: _slink ( lp leveldif -- lp )\n\
	\  0 u+do 1 cells + @ loop\n\
	\;\n\
	\: _slink-cell ( lp leveldif -- cell )\n\
	\  _slink 2 cells + @\n\
	\;\n\
	\: _mkstring\n\
	\  here 2 cells allot dup 3 roll 3 roll rot 2!\n\
	\;\n\
	\: _mktuple ( eu .. e1 u -- tupla )\n\
	\dup here swap cells allot swap\n\
	\0 do\n\
	\  dup i cells + rot swap !\n\
	\  loop\n\
	\;\n"
