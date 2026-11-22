100 REMark Build sort and verify a text concordance
110 IMPLICIT% char_i,check_i,counted_i,entry_i,failed_i,found_i,frequency_i,item_i
120 IMPLICIT% letter_i,line_i,pass_i,passes_i,place_i,position_i,scan_i,sort_count_i
130 IMPLICIT% total_words_i,used_i
140 passes_i = 4
150 REMark BENCHMARK_START
160 DIM corpus$(11,100), dictionary$(127,24), frequency_i(127)
170 DIM line$(101), token$(24), sort_word$(24)
180 check_i = 0
190 failed_i = 0
200 load_corpus
210 FOR pass_i = 1 TO passes_i
220   used_i = 0
230   total_words_i = 0
240   FOR line_i = 0 TO 11
250     line$ = corpus$(line_i) & " "
260     token$ = ""
270     FOR position_i = 1 TO LEN(line$)
280       char_i = CODE(line$(position_i))
290       IF char_i >= 65 AND char_i <= 90 THEN char_i = char_i + 32
300       IF char_i >= 97 AND char_i <= 122 THEN
310         token$ = token$ & CHR$(char_i)
320       ELSE
330         IF LEN(token$) > 0 THEN add_word : token$ = ""
340       END IF
350     END FOR position_i
360   END FOR line_i
370   sort_index
380   verify_index
390   FOR entry_i = 0 TO used_i-1
400     check_i = (check_i + frequency_i(entry_i) * (entry_i+1)) MOD 10000
410     FOR letter_i = 1 TO LEN(dictionary$(entry_i))
420       check_i = (check_i + CODE(dictionary$(entry_i,letter_i)) * letter_i) MOD 10000
430     END FOR letter_i
440   END FOR entry_i
450 END FOR pass_i
460 IF failed_i THEN check_i = -1
470 REMark BENCHMARK_END
480 PRINT "concordance:";check_i
490 STOP
500 REMark Corpus is original text, fixed for reproducible in-memory processing.
510 DEFine PROCedure load_corpus
520   corpus$(0) = "The quick brown fox visits the quiet river. The river carries silver light."
530   corpus$(1) = "A small compiler reads a program and turns the program into machine code."
540   corpus$(2) = "Good measurements repeat the same work and check every result."
550   corpus$(3) = "Arrays store the map. Procedures search the map and return a route."
560   corpus$(4) = "The garden has red flowers, blue flowers, and a very old apple tree."
570   corpus$(5) = "At dawn the birds sing. At dusk the same birds settle beside the river."
580   corpus$(6) = "A word index counts words, sorts words, and preserves their frequencies."
590   corpus$(7) = "Native code can run quickly. Correct code must also preserve its meaning."
600   corpus$(8) = "Read each line, separate its words, then add each word to the index."
610   corpus$(9) = "The route crosses a bridge and follows the river back to the garden."
620   corpus$(10) = "Numbers and names describe the world; programs make those descriptions useful."
630   corpus$(11) = "Measure once, measure twice, and always compare the answers before the timings."
640 END DEFine
650 REMark Insert into an unsorted dictionary while retaining occurrence counts.
660 DEFine PROCedure add_word
670   LOCal found_i, scan_i
680   found_i = -1
690   FOR scan_i = 0 TO used_i-1
700     IF dictionary$(scan_i) = token$ THEN found_i = scan_i : EXIT scan_i
710   END FOR scan_i
720   IF found_i = -1 THEN
730     IF used_i >= 128 THEN failed_i = 1 : RETurn
740     found_i = used_i
750     used_i = used_i + 1
760     dictionary$(found_i) = token$
770     frequency_i(found_i) = 0
780   END IF
790   frequency_i(found_i) = frequency_i(found_i) + 1
800   total_words_i = total_words_i + 1
810 END DEFine
820 REMark Stable insertion sort moves each word and its occurrence count together.
830 DEFine PROCedure sort_index
840   LOCal item_i, place_i, sort_count_i
850   FOR item_i = 1 TO used_i-1
860     sort_word$ = dictionary$(item_i)
870     sort_count_i = frequency_i(item_i)
880     place_i = item_i-1
890     REPeat shift_word
900       IF place_i < 0 THEN EXIT shift_word
910       IF dictionary$(place_i) <= sort_word$ THEN EXIT shift_word
920       dictionary$(place_i+1) = dictionary$(place_i)
930       frequency_i(place_i+1) = frequency_i(place_i)
940       place_i = place_i-1
950     END REPeat shift_word
960     dictionary$(place_i+1) = sort_word$
970     frequency_i(place_i+1) = sort_count_i
980   END FOR item_i
990 END DEFine
1000 REMark Check ordering, dictionary cardinality, and conservation of word counts.
1010 DEFine PROCedure verify_index
1020   LOCal scan_i, counted_i
1030   counted_i = 0
1040   FOR scan_i = 0 TO used_i-1
1050     counted_i = counted_i + frequency_i(scan_i)
1060     IF frequency_i(scan_i) < 1 THEN failed_i = 1
1070     IF scan_i > 0 THEN
1080       IF dictionary$(scan_i-1) >= dictionary$(scan_i) THEN failed_i = 1
1090     END IF
1100   END FOR scan_i
1110   IF counted_i <> total_words_i THEN failed_i = 1
1120   IF used_i <> 97 OR total_words_i <> 147 THEN failed_i = 1
1130 END DEFine
