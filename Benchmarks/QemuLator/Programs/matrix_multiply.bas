100 REMark Eight by eight integer matrix multiplication and weighted checksum
110 IMPLICIT% a_i,b_i,c_i,check_i,col_i,k_i,pass_i,passes_i
120 IMPLICIT% row_i,total_i
130 passes_i = 10
140 REMark BENCHMARK_START
150 DIM a_i(7,7), b_i(7,7), c_i(7,7)
160 check_i = 0
170 FOR pass_i = 1 TO passes_i
180 FOR row_i = 0 TO 7
190 FOR col_i = 0 TO 7
200 a_i(row_i,col_i) = (row_i + col_i + pass_i) MOD 7
210 b_i(row_i,col_i) = (row_i + 2 * col_i + pass_i) MOD 5
220 END FOR col_i
230 END FOR row_i
240 FOR row_i = 0 TO 7
250 FOR col_i = 0 TO 7
260 total_i = 0
270 FOR k_i = 0 TO 7
280 total_i = total_i + a_i(row_i,k_i) * b_i(k_i,col_i)
290 END FOR k_i
300 c_i(row_i,col_i) = total_i
310 END FOR col_i
320 END FOR row_i
330 FOR row_i = 0 TO 7
340 FOR col_i = 0 TO 7
350 check_i = (check_i + c_i(row_i,col_i) * (1 + ((row_i + col_i) MOD 7))) MOD 10000
360 END FOR col_i
370 END FOR row_i
380 END FOR pass_i
390 IF passes_i = 10 AND check_i <> 1094 THEN PRINT "FAIL matrix_multiply checksum" : STOP
400 REMark BENCHMARK_END
410 PRINT "matrix_multiply:"; check_i
420 STOP
