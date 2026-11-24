10 REMark Procedure-local FOR counter; benchmark separately from applications
20 passes% = 20
30 check% = 0
40 FOR pass% = 1 TO passes%
50   calculate (pass%),(1000)
60   check% = (check% + value%) MOD 10000
70 END FOR pass%
80 IF passes% = 20 AND check% <> 270 THEN PRINT "FAIL local_integer_for checksum" : STOP
90 PRINT "local_integer_for:"; check%
100 STOP
110 DEFine PROCedure calculate(seed%,last%)
120   LOCal i%,sum%
130   sum% = seed%
140   FOR i% = 1 TO last%
150     sum% = (sum% + i% MOD 7) MOD 8192
160   END FOR i%
170   value% = sum%
180 END DEFine
