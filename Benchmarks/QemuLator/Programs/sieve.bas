100 REMark Sieve of Eratosthenes through 1000 with array writes and branches
110 IMPLICIT% check_i,count_i,divisor_i,flags_i,i_i,multiple_i,pass_i,passes_i
120 passes_i = 5
130 REMark BENCHMARK_START
140 DIM flags_i(1000)
150 check_i = 0
160 FOR pass_i = 1 TO passes_i
170 FOR i_i = 0 TO 1000
180 flags_i(i_i) = 1
190 END FOR i_i
200 flags_i(0) = 0 : flags_i(1) = 0
210 FOR divisor_i = 2 TO 31
220 IF flags_i(divisor_i) THEN
230 FOR multiple_i = divisor_i * divisor_i TO 1000 STEP divisor_i
240 flags_i(multiple_i) = 0
250 END FOR multiple_i
260 END IF
270 END FOR divisor_i
280 count_i = 0
290 FOR i_i = 2 TO 1000
300 IF flags_i(i_i) THEN count_i = count_i + 1
310 END FOR i_i
320 check_i = (check_i + count_i) MOD 10000
330 END FOR pass_i
340 IF passes_i = 5 AND check_i <> 840 THEN PRINT "FAIL sieve checksum" : STOP
350 REMark BENCHMARK_END
360 PRINT "sieve:"; check_i
370 STOP
