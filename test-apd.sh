#!/bin/sh

echo "(apd '(q w e) ???) :"
(cat apd.kln ; echo "(xs) ((q w e))") | ./partial-evaluator.scm
echo ""

echo "(apd ??? '(q w e)) :"
(cat apd.kln ; echo "(ys) ((q w e))") | ./partial-evaluator.scm
echo ""

