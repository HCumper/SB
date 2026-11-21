10 REMark Integer arithmetic and MOD in a counted loop
20 passes% = 20
30 check% = 0
40 FOR pass% = 1 TO passes%
50   x% = pass%
60   FOR i% = 1 TO 1000
70     x% = (x% + (i% MOD 97) * 3 + 1) MOD 8192
80   END FOR i%
90   check% = (check% + x%) MOD 10000
100 END FOR pass%
110 IF passes% = 20 AND check% <> 6430 THEN PRINT "FAIL integer_arithmetic checksum" : STOP
120 PRINT "integer_arithmetic:"; check%
130 STOP
