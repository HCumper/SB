100 REMark Integer arithmetic and MOD in a counted loop
110 IMPLICIT% check_i,i_i,pass_i,passes_i,x_i
120 passes_i = 20
130 REMark BENCHMARK_START
140 check_i = 0
150 FOR pass_i = 1 TO passes_i
160 x_i = pass_i
170 FOR i_i = 1 TO 1000
180 x_i = (x_i + (i_i MOD 97) * 3 + 1) MOD 8192
190 END FOR i_i
200 check_i = (check_i + x_i) MOD 10000
210 END FOR pass_i
220 IF passes_i = 20 AND check_i <> 6430 THEN PRINT "FAIL integer_arithmetic checksum" : STOP
230 REMark BENCHMARK_END
240 PRINT "integer_arithmetic:"; check_i
250 STOP
