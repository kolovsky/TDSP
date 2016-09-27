Time Dependent Shortest path problem
==========================

This project is focused on Time-Dependent shortest path problem in MapReduce especially project wants solve problem EA_s*(*) - Earliest Departure for all time.
Now it is implemented Label Correcting algorithm with priority queue based on minimum arrival time in node.
Algorithm developed by (DEAN 2004) is in implementation progress.

I future I will plan implement all relevant state of the art (e.g (DEHNE 2012), (DING 2008, ...)

Now is implemented:
* Modificed dijkstra for dynamic problem, EA_s*(t) - Earliest Arrival for one time, O(E + N log N)
* Reverse Dinamic Dijkstra algorithm, LD_*t(t) - Least Departure for one time, O(E + N log N)
* Label correcting algorithm (LCA) with PriorityQueue (compare minimum travel time), EA_s*(*) - Earliest Departure for all time

(DEAN 2004) Dean, B. C. Shortest paths in FIFO time-dependent networks: Theory and algorithms Rapport technique, Massachusetts Institute of Technology, 2004

(DEHNE 2012) Dehne, F.; Omran, M. T. & Sack, J.-R. Shortest paths in time-dependent FIFO networks Algorithmica, Springer, 2012, 62, 416-435

(DING 2008) Ding, B.; Yu, J. X. & Qin, L. Finding time-dependent shortest paths over large graphs Proceedings of the 11th international conference on Extending database technology: Advances in database technology, 2008, 205-216
