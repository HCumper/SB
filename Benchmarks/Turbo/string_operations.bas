10 REMark Build and rotate a 64 character string with concatenation slicing LEN and CODE
20 passes% = 20
30 DIM text$(80)
40 check% = 0
50 FOR pass% = 1 TO passes%
60   IF pass% MOD 2 = 1 THEN piece$ = "ab" : ELSE piece$ = "cd"
70   text$ = ""
80   FOR i% = 1 TO 32
90     text$ = text$ & piece$
100   END FOR i%
110   FOR i% = 1 TO 32
120     text$ = text$(2 TO) & text$(1)
130   END FOR i%
140   result% = LEN(text$) + CODE(text$(1)) + CODE(text$(64))
150   check% = (check% + result%) MOD 10000
160 END FOR pass%
170 IF passes% = 20 AND check% <> 5220 THEN PRINT "FAIL string_operations checksum" : STOP
180 PRINT "string_operations:"; check%
190 STOP
