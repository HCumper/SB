10 REMark Allocation microbenchmark: bounded arithmetic chains, not a representative application
20 passes% = 20
30 check% = 0
40 FOR pass% = 1 TO passes%
50   FOR i% = 1 TO 1000
60     a% = i% * 3 : b% = a% + 7 : c% = b% - 2 : d% = -c%
70   END FOR i%
80   check% = (check% + (-d%) + pass%) MOD 10000
90 END FOR pass%
100 IF passes% = 20 AND check% <> 310 THEN PRINT "FAIL regalloc_arithmetic checksum" : STOP
110 PRINT "regalloc_arithmetic:"; check%
120 STOP
