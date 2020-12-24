# Unix makefile for ccalcmain example

HOME=/usr/local
MOSMLHOME=${HOME}/mosml
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

GRALOBJS=ccalcgrm.uo ccalclex.uo ccalcast.uo ccalcemit.uo ccalc.uo

all: ccalc

ccalc: $(GRALOBJS) $(OBJSGEN)
	$(MOSMLL) -o ccalc $(EXEFILE) ccalc.uo

ccalcgrm.sml ccalcgrm.sig: ccalcgrm.y 
	$(MOSMLYACC) ccalcgrm.y

ccalclex.sml: ccalclex.lex
	$(MOSMLLEX) ccalclex.lex

clean:
	$(REMOVE) Makefile.bak
	$(REMOVE) ccalcgrm.output
	$(REMOVE) ccalcgrm.sig
	$(REMOVE) ccalcgrm.sml
	$(REMOVE) ccalclex.sml
	$(REMOVE) ccalc
	$(REMOVE) *.ui
	$(REMOVE) *.uo

.sig.ui:
	$(MOSMLC) $<

.sml.uo:
	$(MOSMLC) $<

depend: ccalclex.sml ccalcgrm.sml ccalcast.sml ccalcemit.sml ccalc.sml
	$(REMOVE) Makefile.bak
	$(MOVE) Makefile Makefile.bak
	$(MOSMLTOOLS)/cutdeps < Makefile.bak > Makefile
	$(MOSMLTOOLS)/mosmldep >> Makefile

### DO NOT DELETE THIS LINE
ccalcgrm.ui: ccalcast.uo 
ccalcgrm.uo: ccalcgrm.ui ccalcast.uo 
ccalclex.uo: ccalcgrm.ui 
ccalcemit.uo: ccalcast.uo 
ccalc.uo: ccalcgrm.ui ccalclex.uo ccalcemit.uo 
