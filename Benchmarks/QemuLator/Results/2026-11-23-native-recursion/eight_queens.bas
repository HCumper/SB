100 REMark Enumerate and validate every eight-queens solution
110 IMPLICIT% board_sum_i,check_i,choice_i,code_i,col_i,column_i,down_i,failed_i
120 IMPLICIT% falling_i,first_i,left_i,pass_i,passes_i,queen_i,right_i,rising_i
130 IMPLICIT% row_i,separation_i,slot_i,solutions_i,up_i
140 passes_i = 1
150 REMark BENCHMARK_START
160 DIM queen_i(8), column_i(8), rising_i(16), falling_i(16), first_i(8)
170 check_i = 0
180 failed_i = 0
190 FOR pass_i = 1 TO passes_i
200   clear_board
210   solutions_i = 0
220   board_sum_i = 0
230   search_board 1
240   IF solutions_i <> 92 THEN failed_i = 1
250   FOR col_i = 1 TO 8
260     IF first_i(col_i) <> first_i(9-col_i) THEN failed_i = 1
270     board_sum_i = (board_sum_i + first_i(col_i) * col_i) MOD 10000
280   END FOR col_i
290   check_i = (check_i + board_sum_i + solutions_i) MOD 10000
300 END FOR pass_i
310 IF failed_i THEN check_i = -1
320 REMark BENCHMARK_END
330 PRINT "eight_queens:";check_i
340 STOP
350 REMark Reset occupancy maps before enumerating every eight-queen placement.
360 DEFine PROCedure clear_board
370   LOCal slot_i
380   FOR slot_i = 0 TO 8
390     queen_i(slot_i) = 0
400     column_i(slot_i) = 0
410     first_i(slot_i) = 0
420   END FOR slot_i
430   FOR slot_i = 0 TO 16
440     rising_i(slot_i) = 0
450     falling_i(slot_i) = 0
460   END FOR slot_i
470 END DEFine
480 REMark Recursive backtracking uses independent column and diagonal maps.
490 DEFine PROCedure search_board(row_i)
500   LOCal choice_i, up_i, down_i
510   IF row_i = 9 THEN record_board : RETurn
520   FOR choice_i = 1 TO 8
530     up_i = row_i + choice_i
540     down_i = row_i - choice_i + 8
550     IF column_i(choice_i) = 0 AND rising_i(up_i) = 0 AND falling_i(down_i) = 0 THEN
560       queen_i(row_i) = choice_i
570       column_i(choice_i) = 1
580       rising_i(up_i) = 1
590       falling_i(down_i) = 1
600       search_board (row_i+1)
610       column_i(choice_i) = 0
620       rising_i(up_i) = 0
630       falling_i(down_i) = 0
640     END IF
650   END FOR choice_i
660 END DEFine
670 REMark Verify complete boards without relying on the occupancy maps.
680 DEFine PROCedure record_board
690   LOCal left_i, right_i, separation_i, code_i
700   code_i = 0
710   FOR left_i = 1 TO 8
720     code_i = (code_i + queen_i(left_i) * left_i * left_i) MOD 10000
730     FOR right_i = left_i+1 TO 8
740       separation_i = ABS(queen_i(left_i)-queen_i(right_i))
750       IF separation_i = 0 OR separation_i = right_i-left_i THEN failed_i = 1
760     END FOR right_i
770   END FOR left_i
780   solutions_i = solutions_i + 1
790   first_i(queen_i(1)) = first_i(queen_i(1)) + 1
800   board_sum_i = (board_sum_i + code_i) MOD 10000
810 END DEFine
