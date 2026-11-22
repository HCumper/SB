10 REMark Native-value microbenchmark: repeated DIVMOD and arithmetic results, not a representative application
20 passes% = 20
30 check% = 0
40 FOR pass% = 1 TO passes%
50   FOR i% = 1 TO 1000
60     value% = ((i% MOD 97) + 1) * ((i% MOD 97) + 1)
70   END FOR i%
80   check% = (check% + value% + pass%) MOD 10000
90 END FOR pass%
100 IF passes% = 20 AND check% <> 9430 THEN PRINT "FAIL native_repeated_expression checksum" : STOP
110 PRINT "native_repeated_expression:"; check%
120 STOP
