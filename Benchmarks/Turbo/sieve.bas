10 REMark Sieve of Eratosthenes through 1000 with array writes and branches
20 passes% = 5
30 DIM flags%(1000)
40 check% = 0
50 FOR pass% = 1 TO passes%
60   FOR i% = 0 TO 1000
70     flags%(i%) = 1
80   END FOR i%
90   flags%(0) = 0 : flags%(1) = 0
100   FOR divisor% = 2 TO 31
110     IF flags%(divisor%) THEN
120       FOR multiple% = divisor% * divisor% TO 1000 STEP divisor%
130         flags%(multiple%) = 0
140       END FOR multiple%
150     END IF
160   END FOR divisor%
170   count% = 0
180   FOR i% = 2 TO 1000
190     IF flags%(i%) THEN count% = count% + 1
200   END FOR i%
210   check% = (check% + count%) MOD 10000
220 END FOR pass%
230 IF passes% = 5 AND check% <> 840 THEN PRINT "FAIL sieve checksum" : STOP
240 PRINT "sieve:"; check%
250 STOP
