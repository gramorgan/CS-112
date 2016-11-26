#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/gprolog/bin/gprolog --consult-file
/*
 * Ankur Goswami (agoswam3@ucsc.edu)
 * Morgan Grant (mlgrant@ucsc.edu)
 */

:- initialization(main).

/* Degrees, minutes to radians */
dtor(d) :-
   write(d),
   nl.

main :-
   dtor('60'),
   halt.

