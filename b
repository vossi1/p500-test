#!/bin/sh
acme -v p2-p.b
diff -s p2-p.prg fix02-screen/p2-p.prg
cmp p2-p.prg fix02-screen/p2-p.prg