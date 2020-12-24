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

OBJS= ffuncast.uo ffuncenv.uo ffuncgrm.uo ffunclex.uo ffunclifting.uo \
		ffuncemit.uo ffuncmain.uo

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

depend: ffuncast.sml ffuncgrm.sml ffunclex.sml ffuncemit.sml \
		ffuncenv.sml ffunclifting.sml ffuncmain.sml
	$(REMOVE) Makefile.bak
	$(MOVE) Makefile Makefile.bak
	$(MOSMLTOOLS)/cutdeps < Makefile.bak > Makefile
	$(MOSMLTOOLS)/mosmldep >> Makefile

### DO NOT DELETE THIS LINE
ffunclex.uo: ffuncgrm.ui 
ffuncgrm.ui: ffuncast.uo 
ffuncemit.uo: ffuncast.uo ffuncenv.uo 
ffuncenv.uo: ffuncast.uo 
ffuncgrm.uo: ffuncgrm.ui ffuncast.uo 
ffuncmain.uo: ffunclifting.uo ffuncgrm.ui ffuncast.uo ffuncemit.uo \
    ffunclex.uo ffuncenv.uo 
ffunclifting.uo: ffuncast.uo ffuncenv.uo 
