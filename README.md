# DERC-Eliminates-Redundant-Computations
some experiments on [first order lisp] partial evaluator with guaranteed termination

the ultimate goal was to use 2-level interpreter for drcz2 language (lexically scoped minimal lisp)
in order to compile its programs with some twists into form digestable by i.a. drcz0->DRC compiler or something something.

try ./test-apd.sh

kleene-defs.scm describes the subject language.

in order to compile similar language with lambdas (kln.kln) one needs to uncomment the dirty hack in peval-memory-and-stuff.scm

no worries, it still should always terminate [just the trick relies on particular function/argument names for now].

the interpreter kln.kln should be improved with some cheap tricks.

tbc [or maybe not]

