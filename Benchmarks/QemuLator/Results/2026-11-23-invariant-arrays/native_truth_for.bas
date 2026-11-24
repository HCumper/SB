10 REMark Integer truth and eager AND branches; report as a microbenchmark
20 passes% = 20
30 check% = 0
40 FOR pass% = 1 TO passes%
50   calculate (pass%),(1000)
60   check% = (check% + value%) MOD 10000
70 END FOR pass%
80 IF passes% = 20 AND check% <> 4970 THEN PRINT "FAIL native_truth_for checksum" : STOP
90 PRINT "native_truth_for:"; check%
100 STOP
110 DEFine PROCedure calculate(seed%,last%)
120   LOCal i%,sum%,truth%
130   sum% = seed%
140   FOR i% = 1 TO last%
150     truth% = (i% MOD 3) - 1
160     IF truth% THEN sum% = (sum% + 2) MOD 8192 : ELSE sum% = (sum% + 1) MOD 8192
170     IF truth% AND (i% MOD 7 <> 0) THEN sum% = (sum% + 1) MOD 8192
180   END FOR i%
190   value% = sum%
200 END DEFine
