# $Id: Makefile,v 1.15 2014-11-17 15:28:57-08 - - $

MKFILE    = Makefile
DEPSFILE  = ${MKFILE}.deps
NOINCLUDE = ci clean spotless
NEEDINCL  = ${filter ${NOINCLUDE}, ${MAKECMDGOALS}}
SUBMAKE   = ${MAKE} --no-print-directory

SOURCE    = bigint.mli bigint.ml maindc.ml scanner.mll
ALLSRC    = ${SOURCE} dc.ml ${MKFILE}
OBJCMO    = bigint.cmo scanner.cmo maindc.cmo
OBJCMI    = ${patsubst %.cmo, %.cmi, ${OBJCMO}}
CAMLRUN   = ocamldc
LISTING   = Listing.ps

all : ${CAMLRUN}

${CAMLRUN} : ${OBJCMO} ${OBJCMI}
	ocamlc ${OBJCMO} -g -o ${CAMLRUN}

%.cmi : %.mli
	ocamlc -g -c $<

%.cmo : %.ml
	ocamlc -g -c $<

%.ml : %.mll
	ocamllex $<

clean :
	- rm ${OBJCMO} ${OBJCMI} ${DEPSFILE} scanner.ml

spotless : clean
	- rm ${CAMLRUN} ${LISTING} ${LISTING:.ps=.pdf}

ci : ${RCSFILES}
	cid + ${ALLSRC}
	checksource ${ALLSRC}

deps : ${SOURCE}
	ocamldep ${SOURCE} >${DEPSFILE}

${DEPSFILE} :
	@ touch ${DEPSFILE}
	${SUBMAKE} deps

lis : ${ALLSRC}
	mkpspdf ${LISTING} ${ALLSRC} ${DEPSFILE}

again :
	${SUBMAKE} spotless ci deps
	${SUBMAKE} all lis

ifeq (${NEEDINCL}, )
include ${DEPSFILE}
endif

.PRECIOUS : scanner.ml

