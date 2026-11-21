10 REMark Optimizer microbenchmark with literal expressions and integer identities
20 passes% = 20
30 check% = 0
40 FOR pass% = 1 TO passes%
50   x% = pass%
60   FOR i% = 1 TO 1000
70     x% = (x% + (2 + 3) * 4 + (i% + 0) * 1) MOD 8192
80   END FOR i%
90   check% = (check% + x%) MOD 10000
100 END FOR pass%
110 IF passes% = 20 AND check% <> 8290 THEN PRINT "FAIL constant_expressions checksum" : STOP
120 PRINT "constant_expressions:"; check%
130 STOP
