10 REMark Exact dyadic floating additions scaling and conversion to integer
20 passes% = 20
30 check% = 0
40 FOR pass% = 1 TO passes%
50   x = pass%
60   FOR i% = 1 TO 1000
70     x = x + 0.125
80   END FOR i%
90   result% = x * 2.0 + 1.0
100   check% = (check% + result%) MOD 10000
110 END FOR pass%
120 IF passes% = 20 AND check% <> 5440 THEN PRINT "FAIL float_dyadic checksum" : STOP
130 PRINT "float_dyadic:"; check%
140 STOP
