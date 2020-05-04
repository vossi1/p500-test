#!/bin/sh
acme -v p2-p.b
diff -s p2-p.prg fix09-untested/p2-p.prg
cmp p2-p.prg fix09-untested/p2-p.prg