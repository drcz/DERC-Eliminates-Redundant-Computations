#!/bin/sh
echo "kln (((^ (x) (* x x)) (+ 2 y))) y=3 [~23s]"
 (cat kln.kln ; echo "(names vals) ((y) (3))") | ./partial-evaluator.scm 
echo "---"
echo "kln (((^ (x) (* x x)) (+ 2 y))) y=??? [~25s]"
 (cat kln.kln ; echo "(names) ((y))") | ./partial-evaluator.scm 
