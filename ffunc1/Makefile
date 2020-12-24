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

OBJS= ffunctipo.uo ffuncast.uo ffuncenv.uo ffuncgrm.uo ffunclex.uo \
		ffunclifting.uo ffuncemit.uo ffunctipado.uo ffuncrt.uo ffuncmain.uo

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
		ffuncenv.sml ffunclifting.sml ffunctipado.sml ffuncrt.sml ffuncmain.sml
	$(REMOVE) Makefile.bak
	$(MOVE) Makefile Makefile.bak
	$(MOSMLTOOLS)/cutdeps < Makefile.bak > Makefile
	$(MOSMLTOOLS)/mosmldep >> Makefile

### DO NOT DELETE THIS LINE
ffunclifting.uo: ffuncast.uo ffuncenv.uo 
ffuncmain.uo: ffuncrt.uo ffuncgrm.ui ffunclex.uo ffunctipado.uo ffuncenv.uo 
ffunctipado.uo: ffuncast.uo ffuncemit.uo ffunctipo.uo ffuncenv.uo 
ffuncenv.uo: ffuncast.uo ffunctipo.uo 
ffuncast.uo: ffunctipo.uo 
ffuncemit.uo: ffuncast.uo 
ffuncgrm.uo: ffuncgrm.ui ffuncast.uo ffunctipo.uo 
ffuncgrm.ui: ffuncast.uo ffunctipo.uo 
ffunclex.uo: ffuncgrm.ui ffunctipo.uo 
