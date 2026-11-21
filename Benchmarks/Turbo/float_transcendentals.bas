10 REMark ROM SIN COS and SQRT with the trigonometric identity as a numerical check
20 passes% = 5
30 check% = 0
40 FOR pass% = 1 TO passes%
50   FOR i% = 1 TO 256
60     angle = (i% + pass%) / 1024.0
70     sine = SIN(angle) : cosine = COS(angle)
80     root = SQRT(sine * sine + cosine * cosine)
90     IF root < 0.99999 OR root > 1.00001 THEN PRINT "FAIL float_transcendentals result" : STOP
100     check% = (check% + 1) MOD 10000
110   END FOR i%
120 END FOR pass%
130 IF passes% = 5 AND check% <> 1280 THEN PRINT "FAIL float_transcendentals checksum" : STOP
140 PRINT "float_transcendentals:"; check%
150 STOP
