10 REMark Integer IF/ELSE inside a procedure-local FOR; report separately from applications
20 passes% = 20
30 check% = 0
40 FOR pass% = 1 TO passes%
50   calculate (pass%),(1000)
60   check% = (check% + value%) MOD 10000
70 END FOR pass%
80 IF passes% = 20 AND check% <> 6870 THEN PRINT "FAIL native_conditional_for checksum" : STOP
90 PRINT "native_conditional_for:"; check%
100 STOP
110 DEFine PROCedure calculate(seed%,last%)
120   LOCal i%,sum%
130   sum% = seed%
140   FOR i% = 1 TO last%
150     IF i% MOD 3 = 0 THEN
160       sum% = (sum% + 2) MOD 8192
170     ELSE
180       sum% = (sum% + 1) MOD 8192
190     END IF
200   END FOR i%
210   value% = sum%
220 END DEFine
