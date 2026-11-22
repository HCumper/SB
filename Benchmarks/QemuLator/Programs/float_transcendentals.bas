100 REMark ROM SIN COS and SQRT with the trigonometric identity as a numerical check
110 IMPLICIT% check_i,i_i,pass_i,passes_i
120 passes_i = 5
130 REMark BENCHMARK_START
140 check_i = 0
150 FOR pass_i = 1 TO passes_i
160 FOR i_i = 1 TO 256
170 angle = i_i + pass_i : angle = angle / 1024.0
180 sine = SIN(angle) : cosine = COS(angle)
190 root = SQRT(sine * sine + cosine * cosine)
200 IF root < 0.99999 OR root > 1.00001 THEN PRINT "FAIL float_transcendentals result" : STOP
210 check_i = (check_i + 1) MOD 10000
220 END FOR i_i
230 END FOR pass_i
240 IF passes_i = 5 AND check_i <> 1280 THEN PRINT "FAIL float_transcendentals checksum" : STOP
250 REMark BENCHMARK_END
260 PRINT "float_transcendentals:"; check_i
270 STOP
