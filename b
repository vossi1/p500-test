#!/bin/sh
acme -Wtype-mismatch -r p2-p.lst -l p2-p.sym p2-p.b
#diff -s p2-p.prg p2-p_old.prg~
#cmp p2-p.prg p2-p_old.prg~