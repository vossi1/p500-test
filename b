#!/bin/sh
acme -v p2-p.b
diff -s p2-p.prg fix07-counter/p2-p.prg
cmp p2-p.prg fix07-counter/p2-p.prg