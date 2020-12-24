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

OBJS= funcast.uo funcenv.uo funcgrm.uo funclex.uo funcrt.uo funcemit.uo funcmain.uo

all: func

func: $(OBJS)
	$(MOSMLL) -o func$(EXEFILE) $(OBJS)

funcgrm.sml funcgrm.sig: funcgrm.y
	$(MOSMLYACC) funcgrm.y

funclex.sml: funclex.lex
	$(MOSMLLEX) funclex.lex

clean:
	$(REMOVE) Makefile.bak
	$(REMOVE) funcgrm.sig
	$(REMOVE) funcgrm.sml
	$(REMOVE) funcgrm.output
	$(REMOVE) funclex.sml
	$(REMOVE) func
	$(REMOVE) *.ui
	$(REMOVE) *.uo

.sig.ui:
	$(MOSMLC) $<

.sml.uo:
	$(MOSMLC) $<

depend: funcast.sml funcgrm.sml funclex.sml funcrt.sml funcemit.sml \
		funcenv.sml funcmain.sml
	$(REMOVE) Makefile.bak
	$(MOVE) Makefile Makefile.bak
	$(MOSMLTOOLS)/cutdeps < Makefile.bak > Makefile
	$(MOSMLTOOLS)/mosmldep >> Makefile

### DO NOT DELETE THIS LINE
funclex.uo: funcgrm.ui 
funcemit.uo: funcenv.uo funcast.uo 
funcgrm.uo: funcgrm.ui funcast.uo 
funcmain.uo: funcrt.uo funcenv.uo funcemit.uo funcgrm.ui funclex.uo 
funcgrm.ui: funcast.uo 
funcenv.uo: funcast.uo 
