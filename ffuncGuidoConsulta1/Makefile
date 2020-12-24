# Unix makefile for calc example

# MOSMLHOME=${HOME}/mosml
# GUIDO
MOSMLHOME=/usr/local/mosml
MOSMLTOOLS=camlrunm /usr/local/mosml/share/mosml/tools
MOSMLLEX=${MOSMLHOME}/bin/mosmllex
MOSMLYACC=${MOSMLHOME}/bin/mosmlyac -v

MOSMLC=mosmlc -c -liberal
MOSMLL=mosmlc

# Juan PC
HOME=/usr/local/
MOSMLHOME=${HOME}/
MOSMLTOOLS=camlrunm /usr/local/share/mosml/tools
MOSMLLEX=mosmllex
MOSMLYACC=mosmlyac -v

GCC=gcc
CFLAGS= -g -Wall
MOSMLC=${MOSMLHOME}/bin/mosmlc -c -liberal
MOSMLL=${MOSMLHOME}/bin/mosmlc

# Unix
REMOVE=rm -f
MOVE=mv
EXEFILE=

# DOS
#REMOVE=del
#MOVE=move
#EXEFILE=.exe

.SUFFIXES :
.SUFFIXES : .sig .sml .ui .uo

OBJS= ffunctipo.uo ffuncast.uo ffuncenv.uo ffuncgrm.uo ffunclex.uo \
		ffuncemit.uo ffunctipado.uo ffuncrt.uo ffuncmain.uo

all: ffunc

ffunc: $(OBJS)
	$(MOSMLL) -o ffunc$(EXEFILE) $(OBJS)

ffuncgrm.sml ffuncgrm.sig: ffuncgrm.y
	$(MOSMLYACC) ffuncgrm.y

ffunclex.sml: ffunclex.lex
	$(MOSMLLEX) ffunclex.lex

clean:
	$(REMOVE) Makefile.bak
	$(REMOVE) ffuncgrm.sig
	$(REMOVE) ffuncgrm.sml
	$(REMOVE) ffunclex.sml
	$(REMOVE) ffunc
	$(REMOVE) ffuncgrm.output
	$(REMOVE) *.ui
	$(REMOVE) *.uo

.sig.ui:
	$(MOSMLC) $<

.sml.uo:
	$(MOSMLC) $<

depend: ffunctipo.sml ffuncast.sml ffuncgrm.sml ffunclex.sml ffuncemit.sml \
		ffuncenv.sml ffunctipado.sml ffuncrt.sml ffuncmain.sml
	$(REMOVE) Makefile.bak
	$(MOVE) Makefile Makefile.bak
	$(MOSMLTOOLS)/cutdeps < Makefile.bak > Makefile
	$(MOSMLTOOLS)/mosmldep >> Makefile

### DO NOT DELETE THIS LINE
ffunctipado.uo: ffuncast.uo ffuncemit.uo ffunctipo.uo ffuncenv.uo 
ffunclex.uo: ffuncgrm.ui ffunctipo.uo 
ffuncemit.uo: ffuncast.uo 
ffuncgrm.uo: ffuncgrm.ui ffuncast.uo ffunctipo.uo 
ffuncast.uo: ffunctipo.uo 
armatupla0.uo: ffunctipo.uo 
ffuncmain.uo: ffuncrt.uo ffuncgrm.ui ffuncast.uo ffunclex.uo ffunctipado.uo \
    ffuncenv.uo 
ffuncenv.uo: ffuncast.uo ffunctipo.uo 
ffuncgrm.ui: ffuncast.uo ffunctipo.uo 
