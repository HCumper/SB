10 REMark Eight by eight integer matrix multiplication and weighted checksum
20 passes% = 10
30 DIM a%(7,7), b%(7,7), c%(7,7)
40 check% = 0
50 FOR pass% = 1 TO passes%
60   FOR row% = 0 TO 7
70     FOR col% = 0 TO 7
80       a%(row%,col%) = (row% + col% + pass%) MOD 7
90       b%(row%,col%) = (row% + 2 * col% + pass%) MOD 5
100     END FOR col%
110   END FOR row%
120   FOR row% = 0 TO 7
130     FOR col% = 0 TO 7
140       total% = 0
150       FOR k% = 0 TO 7
160         total% = total% + a%(row%,k%) * b%(k%,col%)
170       END FOR k%
180       c%(row%,col%) = total%
190     END FOR col%
200   END FOR row%
210   FOR row% = 0 TO 7
220     FOR col% = 0 TO 7
230       check% = (check% + c%(row%,col%) * (1 + ((row% + col%) MOD 7))) MOD 10000
240     END FOR col%
250   END FOR row%
260 END FOR pass%
270 IF passes% = 10 AND check% <> 1094 THEN PRINT "FAIL matrix_multiply checksum" : STOP
280 PRINT "matrix_multiply:"; check%
290 STOP
