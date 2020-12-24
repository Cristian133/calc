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

OBJS= tfunctipo.uo tfuncast.uo tfuncenv.uo tfuncemit.uo tfunctipado.uo tfuncgrm.uo tfunclex.uo \
		tfuncrt.uo tfuncmain.uo

all: tfunc

tfunc: $(OBJS)
	$(MOSMLL) -o tfunc$(EXEFILE) $(OBJS)

tfuncgrm.sml tfuncgrm.sig: tfuncgrm.y
	$(MOSMLYACC) tfuncgrm.y

tfunclex.sml: tfunclex.lex
	$(MOSMLLEX) tfunclex.lex

clean:
	$(REMOVE) Makefile.bak
	$(REMOVE) tfuncgrm.sig
	$(REMOVE) tfuncgrm.sml
	$(REMOVE) tfuncgrm.output
	$(REMOVE) tfunclex.sml
	$(REMOVE) tfunc
	$(REMOVE) *.ui
	$(REMOVE) *.uo

.sig.ui:
	$(MOSMLC) $<

.sml.uo:
	$(MOSMLC) $<

depend: tfuncast.sml tfunctipo.sml tfunctipado.sml tfuncgrm.sml tfunclex.sml tfuncrt.sml tfuncemit.sml \
		tfuncenv.sml tfuncmain.sml
	$(REMOVE) Makefile.bak
	$(MOVE) Makefile Makefile.bak
	$(MOSMLTOOLS)/cutdeps < Makefile.bak > Makefile
	$(MOSMLTOOLS)/mosmldep >> Makefile

### DO NOT DELETE THIS LINE
tfuncgrm.ui: tfunctipo.uo tfuncast.uo 
tfuncmain.uo: tfuncrt.uo tfuncemit.uo tfunclex.uo tfuncenv.uo tfuncgrm.ui \
    tfunctipado.uo 
tfuncast.uo: tfunctipo.uo 
tfuncgrm.uo: tfuncgrm.ui tfunctipo.uo tfuncast.uo 
tfunclex.uo: tfunctipo.uo tfuncgrm.ui 
tfuncenv.uo: tfunctipo.uo tfuncast.uo 
tfunctipado.uo: tfuncemit.uo tfunctipo.uo tfuncenv.uo tfuncast.uo 
tfuncemit.uo: tfuncenv.uo tfuncast.uo 
