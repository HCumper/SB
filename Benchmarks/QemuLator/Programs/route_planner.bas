100 REMark Build a road network and verify all shortest routes
110 IMPLICIT% best_i,candidate_i,check_i,cost_i,current_i,destination_i,distance_i,edge_i
120 IMPLICIT% failed_i,from_i,hops_i,node_i,origin_i,other_i,parent_i,pass_i
130 IMPLICIT% passes_i,previous_i,road_i,route_cost_i,source_i,step_i,target_i,to_i
140 IMPLICIT% town_i,visited_i,weight_i
150 passes_i = 1
160 REMark BENCHMARK_START
170 DIM road_i(31,31), distance_i(31), previous_i(31), visited_i(31)
180 check_i = 0
190 failed_i = 0
200 FOR pass_i = 1 TO passes_i
210   build_network
220   FOR origin_i = 0 TO 31
230     shortest_routes (origin_i)
240     verify_routes (origin_i)
250     FOR destination_i = 0 TO 31
260       check_i = (check_i + distance_i(destination_i) * (1 + destination_i MOD 7)) MOD 10000
270     END FOR destination_i
280   END FOR origin_i
290 END FOR pass_i
300 IF failed_i THEN check_i = -1
310 REMark BENCHMARK_END
320 PRINT "route_planner:";check_i
330 STOP
340 REMark A connected ring of towns plus deterministic express-road shortcuts.
350 DEFine PROCedure build_network
360   LOCal town_i, other_i, weight_i
370   FOR town_i = 0 TO 31
380     FOR other_i = 0 TO 31
390       road_i(town_i,other_i) = 0
400     END FOR other_i
410   END FOR town_i
420   FOR town_i = 0 TO 31
430     other_i = (town_i+1) MOD 32
440     weight_i = 3 + town_i MOD 9
450     put_road (town_i),(other_i),(weight_i)
460     other_i = (town_i+7) MOD 32
470     weight_i = 10 + town_i MOD 13
480     put_road (town_i),(other_i),(weight_i)
490   END FOR town_i
500 END DEFine
510 DEFine PROCedure put_road(from_i,to_i,cost_i)
520   road_i(from_i,to_i) = cost_i
530   road_i(to_i,from_i) = cost_i
540 END DEFine
550 REMark Dijkstra selects the nearest unsettled town and relaxes its roads.
560 DEFine PROCedure shortest_routes(source_i)
570   LOCal node_i, step_i, best_i, current_i, candidate_i, edge_i
580   FOR node_i = 0 TO 31
590     distance_i(node_i) = 30000
600     previous_i(node_i) = -1
610     visited_i(node_i) = 0
620   END FOR node_i
630   distance_i(source_i) = 0
640   FOR step_i = 0 TO 31
650     best_i = 30000
660     current_i = -1
670     FOR node_i = 0 TO 31
680       IF visited_i(node_i) = 0 AND distance_i(node_i) < best_i THEN
690         best_i = distance_i(node_i)
700         current_i = node_i
710       END IF
720     END FOR node_i
730     IF current_i < 0 THEN failed_i = 1 : RETurn
740     visited_i(current_i) = 1
750     FOR node_i = 0 TO 31
760       edge_i = road_i(current_i,node_i)
770       IF visited_i(node_i) = 0 AND edge_i > 0 THEN
780         candidate_i = best_i + edge_i
790         IF candidate_i < distance_i(node_i) THEN
800           distance_i(node_i) = candidate_i
810           previous_i(node_i) = current_i
820         END IF
830       END IF
840     END FOR node_i
850   END FOR step_i
860 END DEFine
870 REMark Follow each predecessor chain and independently total the route cost.
880 DEFine PROCedure verify_routes(source_i)
890   LOCal target_i, current_i, parent_i, hops_i, route_cost_i, edge_i
900   FOR target_i = 0 TO 31
910     current_i = target_i
920     hops_i = 0
930     route_cost_i = 0
940     REPeat trace_route
950       IF current_i = source_i THEN EXIT trace_route
960       parent_i = previous_i(current_i)
970       IF parent_i < 0 OR hops_i > 31 THEN failed_i = 1 : RETurn
980       edge_i = road_i(current_i,parent_i)
990       IF edge_i <= 0 THEN failed_i = 1 : RETurn
1000       route_cost_i = route_cost_i + edge_i
1010       current_i = parent_i
1020       hops_i = hops_i + 1
1030     END REPeat trace_route
1040     IF route_cost_i <> distance_i(target_i) THEN failed_i = 1
1050   END FOR target_i
1060 END DEFine
