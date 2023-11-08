#!/bin/sh
acme -v p2-p.b
diff -s p2-p.prg p2-p_old.prg~
cmp p2-p.prg p2-p_old.prg~