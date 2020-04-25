#!/bin/sh
acme -v p2-p.b
diff -s p2-p.prg original/p2-p.prg
cmp p2-p.prg original/p2-p.prg