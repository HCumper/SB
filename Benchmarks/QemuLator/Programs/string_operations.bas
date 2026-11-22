100 REMark Build and rotate a 64 character string with concatenation slicing LEN and CODE
110 IMPLICIT% check_i,i_i,pass_i,passes_i,result_i
120 passes_i = 80
130 REMark BENCHMARK_START
140 DIM text$(80),piece$(2)
150 check_i = 0
160 FOR pass_i = 1 TO passes_i
170 IF pass_i MOD 2 = 1 THEN piece$ = "ab" : ELSE piece$ = "cd"
180 text$ = ""
190 FOR i_i = 1 TO 32
200 text$ = text$ & piece$
210 END FOR i_i
220 FOR i_i = 1 TO 32
230 text$ = text$(2 TO) & text$(1)
240 END FOR i_i
250 result_i = LEN(text$) + CODE(text$(1)) + CODE(text$(64))
260 check_i = (check_i + result_i) MOD 10000
270 END FOR pass_i
280 IF passes_i = 80 AND check_i <> 880 THEN PRINT "FAIL string_operations checksum" : STOP
290 REMark BENCHMARK_END
300 PRINT "string_operations:"; check_i
310 STOP
