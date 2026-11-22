100 REMark General QDOS floating division multiplication and Horner polynomial
110 IMPLICIT% check_i,i_i,pass_i,passes_i,result_i
120 passes_i = 4
130 REMark BENCHMARK_START
140 check_i = 0
150 FOR pass_i = 1 TO passes_i
160 total = 0.0
170 FOR i_i = 1 TO 1000
180 x = i_i : x = x / 1000.0
190 y = ((1.25 * x - 0.75) * x + 0.5) * x + 2.0
200 total = total + y
210 END FOR i_i
220 IF total < 2312.98 OR total > 2313.02 THEN PRINT "FAIL float_polynomial result" : STOP
230 result_i = total
240 check_i = (check_i + result_i) MOD 10000
250 END FOR pass_i
260 IF passes_i = 4 AND check_i <> 9252 THEN PRINT "FAIL float_polynomial checksum" : STOP
270 REMark BENCHMARK_END
280 PRINT "float_polynomial:"; check_i
290 STOP
