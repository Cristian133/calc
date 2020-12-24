# Unix makefile for calc example

# MOSMLHOME=${HOME}/mosml
MOSMLHOME=/usr/local/mosml
MOSMLTOOLS=camlrunm $(MOSMLHOME)/tools
MOSMLLEX=${MOSMLHOME}/bin/mosmllex
MOSMLYACC=${MOSMLHOME}/bin/mosmlyac -v

MOSMLC=mosmlc -c -liberal
MOSMLL=mosmlc

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

OBJS = tipoast.uo tipogrm.uo tipolex.uo tipomain.uo

all: tipo

tipo: $(OBJS)
	$(MOSMLL) -o tipo$(EXEFILE) $(OBJS)

tipogrm.sml tipogrm.sig: tipogrm.y
	$(MOSMLYACC) tipogrm.y

tipolex.sml: tipolex.lex
	$(MOSMLLEX) tipolex.lex

clean:
	$(REMOVE) Makefile.bak
	$(REMOVE) tipogrm.sig
	$(REMOVE) tipogrm.sml
	$(REMOVE) tipolex.sml
	$(REMOVE) tipo
	$(REMOVE) tipogrm.output
	$(REMOVE) *.ui
	$(REMOVE) *.uo

.sig.ui:
	$(MOSMLC) $<

.sml.uo:
	$(MOSMLC) $<

depend: tipoast.sml tipogrm.sml tipolex.sml tipomain.sml
	$(REMOVE) Makefile.bak
	$(MOVE) Makefile Makefile.bak
	$(MOSMLTOOLS)/cutdeps < Makefile.bak > Makefile
	$(MOSMLTOOLS)/mosmldep >> Makefile

### DO NOT DELETE THIS LINE
tipogrm.ui: tipoast.uo 
tipomain.uo: tipogrm.ui tipoast.uo tipolex.uo 
tipolex.uo: tipogrm.ui tipoast.uo 
tipogrm.uo: tipogrm.ui tipoast.uo 
