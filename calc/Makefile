# Unix makefile for calcmain example

HOME=/usr/local
MOSMLHOME=${HOME}
MOSMLTOOLS=camlrunm $(MOSMLHOME)/tools
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

GRALOBJS=calcgrm.uo calclex.uo calcast.uo calc.uo

all: calc

calc: $(GRALOBJS) $(OBJSGEN)
	$(MOSMLL) -o calc $(EXEFILE) calc.uo

calcgrm.sml calcgrm.sig: calcgrm.y 
	$(MOSMLYACC) calcgrm.y

calclex.sml: calclex.lex
	$(MOSMLLEX) calclex.lex

clean:
	$(REMOVE) Makefile.bak
	$(REMOVE) calcgrm.output
	$(REMOVE) calcgrm.sig
	$(REMOVE) calcgrm.sml
	$(REMOVE) calclex.sml
	$(REMOVE) calc
	$(REMOVE) *.ui
	$(REMOVE) *.uo

.sig.ui:
	$(MOSMLC) $<

.sml.uo:
	$(MOSMLC) $<

depend: calclex.sml calcgrm.sml calcast.sml calc.sml
	$(REMOVE) Makefile.bak
	$(MOVE) Makefile Makefile.bak
	$(MOSMLTOOLS)/cutdeps < Makefile.bak > Makefile
	$(MOSMLTOOLS)/mosmldep >> Makefile

### DO NOT DELETE THIS LINE
calcgrm.ui: calcast.uo 
calclex.uo: calcgrm.ui 
calc.uo: calcgrm.ui calcast.uo calclex.uo 
calcgrm.uo: calcgrm.ui calcast.uo 
