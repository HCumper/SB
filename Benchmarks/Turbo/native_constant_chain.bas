10 REMark Native-value microbenchmark: constant assignment chains, not a representative application
20 passes% = 20
30 check% = 0
40 FOR pass% = 1 TO passes%
50   FOR i% = 1 TO 1000
60     a% = 3 : b% = a% + 4 : c% = b% * 2 : value% = (i% + c%) MOD 8192
70   END FOR i%
80   check% = (check% + value% + pass%) MOD 10000
90 END FOR pass%
100 IF passes% = 20 AND check% <> 490 THEN PRINT "FAIL native_constant_chain checksum" : STOP
110 PRINT "native_constant_chain:"; check%
120 STOP
