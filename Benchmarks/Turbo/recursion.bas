10 REMark Naive recursive Fibonacci at 12 and 13 with value arguments
20 passes% = 10
30 check% = 0
40 FOR pass% = 1 TO passes%
50   result% = fib%(12 + (pass% MOD 2))
60   check% = (check% + result%) MOD 10000
70 END FOR pass%
80 IF passes% = 10 AND check% <> 1885 THEN PRINT "FAIL recursion checksum" : STOP
90 PRINT "recursion:"; check%
100 STOP
110 DEFine FuNction fib%(n%)
120   IF n% < 2 THEN RETurn n%
130   RETurn fib%(n% - 1) + fib%(n% - 2)
140 END DEFine
