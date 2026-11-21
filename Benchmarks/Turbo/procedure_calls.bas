10 REMark Repeated procedure calls with a reference scalar and a value argument
20 passes% = 20
30 check% = 0
40 FOR pass% = 1 TO passes%
50   value% = pass%
60   FOR i% = 1 TO 1000
70     advance value%, (i% MOD 7)
80   END FOR i%
90   check% = (check% + value%) MOD 10000
100 END FOR pass%
110 IF passes% = 20 AND check% <> 270 THEN PRINT "FAIL procedure_calls checksum" : STOP
120 PRINT "procedure_calls:"; check%
130 STOP
140 DEFine PROCedure advance(n%,delta%)
150   n% = (n% + delta%) MOD 8192
160 END DEFine
