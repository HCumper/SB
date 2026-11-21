10 REMark General QDOS floating division multiplication and Horner polynomial
20 passes% = 4
30 check% = 0
40 FOR pass% = 1 TO passes%
50   total = 0.0
60   FOR i% = 1 TO 1000
70     x = i% / 1000.0
80     y = ((1.25 * x - 0.75) * x + 0.5) * x + 2.0
90     total = total + y
100   END FOR i%
110   IF total < 2312.98 OR total > 2313.02 THEN PRINT "FAIL float_polynomial result" : STOP
120   result% = total
130   check% = (check% + result%) MOD 10000
140 END FOR pass%
150 IF passes% = 4 AND check% <> 9252 THEN PRINT "FAIL float_polynomial checksum" : STOP
160 PRINT "float_polynomial:"; check%
170 STOP
