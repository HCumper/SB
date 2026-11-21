10 REMark Sort 64 deterministic integers and validate ordering and weighted checksum
20 passes% = 5
30 DIM values%(63)
40 check% = 0
50 FOR pass% = 1 TO passes%
60   FOR i% = 0 TO 63
70     values%(i%) = (i% * 73 + pass% * 19) MOD 997
80   END FOR i%
90   FOR last% = 63 TO 1 STEP -1
100     FOR i% = 0 TO last% - 1
110       IF values%(i%) > values%(i% + 1) THEN
120         saved% = values%(i%)
130         values%(i%) = values%(i% + 1)
140         values%(i% + 1) = saved%
150       END IF
160     END FOR i%
170   END FOR last%
180   FOR i% = 0 TO 63
190     IF i% > 0 THEN
200       IF values%(i%) < values%(i% - 1) THEN PRINT "FAIL bubble_sort order" : STOP
210     END IF
220     check% = (check% + values%(i%) * (1 + (i% MOD 7))) MOD 10000
230   END FOR i%
240 END FOR pass%
250 IF passes% = 5 AND check% <> 121 THEN PRINT "FAIL bubble_sort checksum" : STOP
260 PRINT "bubble_sort:"; check%
270 STOP
