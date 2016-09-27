import scala.collection.mutable.{ListBuffer, PriorityQueue, HashMap}

/**
 * Created by kolovsky on 9.11.15.
 */
class Graph(edges: Array[(Int, Int, ArrivalFunction)], operations: Operations) extends Serializable{
  //list of nodes
  var nodes: Array[Node] = Array()
  //index for creating graph
  val hm = HashMap.empty[Int, Node]
  // operation objects
  val o = operations
  //list of edges
  var edgeList: Array[Edge] = Array()

  createGraph(edges)
  def dijkstra(s: Int, interval: (Double, Double)): Array[ArrivalInterval]={
    return Array()
  }

  /**
   * build graph using HashMap
   * @param edges - list of edges (source ID, target ID, ArrivalFunction)
   */
  def createGraph(edges: Array[(Int, Int, ArrivalFunction)]):Unit = {
    val el: ListBuffer[Edge] = ListBuffer()
    var ii = 0
    for (e <- edges) {
      if (e._1 != e._2) {
        var source = hm.get(e._1)
        var target = hm.get(e._2)
        var s: Node = null
        var t: Node = null
        if (source == None) {
          s = new Node()
          s.id = e._1
          hm += (e._1 -> s)
          ii += 1
        }
        else {
          s = source.get
        }
        if (target == None) {
          t = new Node()
          t.id = e._2
          hm += (e._2 -> t)
          ii += 1
        }
        else {
          t = target.get
        }
        var edge = new Edge(s, t, e._3)
        el += edge
        s.edges = s.edges :+ edge
        t.redges = t.redges :+ edge
      }
    }

    // add index in array for every Node
    nodes = hm.toArray.map(_._2)
    for(i <- 0 to nodes.length - 1){
      nodes(i).i = i
    }

    // add index in array for every Edge
    edgeList = el.toArray
    for(i <- 0 to edgeList.length - 1){
      edgeList(i).i = i
    }

  }

  /**
   * Label correcting algorithm (LCA) with PriorityQueue (compare minimum travel time)
   * EA_s*(*) - Earliest Departure for all time
   * @param s - source node
   * @param interval - departure interval
   * @return distance node map
   */
  def LCA(s: Int, interval: (Double, Double)): Array[ArrivalFunction] = {
    var dist: Array[ArrivalFunction] = Array.ofDim[ArrivalFunction](nodes.length)
    val pq: PriorityQueue[(ArrivalFunction, Node)] = PriorityQueue.empty[(ArrivalFunction, Node)](MinOrderNode)

    //inicialization
    for (i <- 0 to nodes.length - 1){
      dist(i) = new ArrivalFunction(Array(), Array())
    }
    val s_node = hm.get(s).get
    dist(s_node.i) = new ArrivalInterval(Array(interval._1, interval._2), Array(interval._1, interval._2))
    pq.enqueue((dist(s_node.i), s_node))
    var count = 0

    //main loop
    while (pq.nonEmpty){
      val n = pq.dequeue()._2

      for (e <- n.edges){
          val c = o.combination(e.af, dist(n.i))
          if (o.equals(c, dist(e.t.i))){
            dist(e.t.i) = o.min(c, dist(e.t.i))
            pq.enqueue((dist(e.t.i), e.t))
            count += 1
          }
      }
    }
    //println("LCA count: "+count)
    return dist
  }

  /**
   * Modificed dijkstra for dynamic problem
   * EA_s*(t) - Earliest Arrival for one time
   * O(E + N log N)
   * @param s - source node
   * @param td - departure time from source node
   * @return - dist map (arrival map array)
   */
  def DSP(s: Int, td: Double): Array[Double] = {
    var dist: Array[Double] = Array.ofDim[Double](nodes.length)
    val pq = PriorityQueue.empty[(Double, Node)](MinOrderNodeStatic)
    //inicialization
    for (i <- 0 to nodes.length - 1){
      dist(i) = Double.PositiveInfinity
    }
    val s_node = hm.get(s).get
    dist(s_node.i) = td
    pq.enqueue((dist(s_node.i),s_node))
    var count = 0

    //main loop
    while (pq.nonEmpty){
      val n = pq.dequeue()._2

      for (e <- n.edges){
        val new_time = e.af.getArrival(dist(n.i))
        if (new_time < dist(e.t.i)){
          dist(e.t.i) = new_time
          pq.enqueue((new_time, e.t))
          count += 1
        }
      }
    }
    println("DSP count: "+count)
    return dist
  }

  /**
   * Algorithm published in paper:
   * Dehne, F.; Omran, M. T. & Sack, J.-R. Shortest paths in time-dependent FIFO networks Algorithmica, Springer, 2012, 62, 416-435
   * EA_st(*)
   * @param s - source node
   * @param t - target node
   * @param interval - departure interval
   * @return - arrival function for node "t"
   */
  def DEHNE(s: Int, t: Int, interval: (Double, Double)): ArrivalFunction = {
    throw new Exception("implement me please!")
  }

  /**
   * Algorithm published in paper:
   * Dean, B. C. Shortest paths in FIFO time-dependent networks: Theory and algorithms Rapport technique, Massachusetts Institute of Technology, 2004
   * EA_s*(*)
   * @param s - source node
   * @param interval - departure interval
   * @return
   */
  def DEAN(s: Int, interval: (Double, Double)): Array[ArrivalFunction] ={
    //inicialization
    val dist: Array[AttrNodeDEAN] = Array.ofDim(nodes.length)
    val edgeAttrArray: Array[AttrEdgeDEAN] = Array.ofDim(edgeList.length)
    for(i <- 0 to dist.length - 1){
      dist(i) = new AttrNodeDEAN()
    }

    for(i <- 0 to edgeAttrArray.length - 1){
      edgeAttrArray(i) = new AttrEdgeDEAN(edgeList(i))
    }

    SPInf(s, dist)
    val pq = PriorityQueue.empty[AttrEdgeDEAN](MaxOrderEdgeZ)
    for (e_attr <- edgeAttrArray){
      e_attr.computeXYZ(dist)
      pq.enqueue(e_attr)
      println(pq)
    }

    var t = Double.NegativeInfinity

    //main loop
    var pp = 0
    while (t < 10 && pp < 10){
      pp += 1
      val ea = pq.max(MaxOrderEdgeZ)
      println(pq.toString())
      val t_new = ea.Z
      println("t_new: "+t_new)
      val Xij = ea.X
      val Yij = ea.Y

      if(t_new == Xij){
        ea.move()
        ea.computeXYZ(dist)
        //pq.enqueue(ea)
        println("move")
      }
      else{
        dist(ea.e.t.i).recordCurrentPiece(ea, t_new)
        println("record")
        for (e_in <- ea.e.t.redges){
          edgeAttrArray(e_in.i).computeXYZ(dist)
          //pq.enqueue(edgeAttrArray(e_in.i))
        }

      }
      t = t_new

    }

    return dist.map(_.getArrivalFunction())
  }
  private def SPInf(s:Int, dist: Array[AttrNodeDEAN]): Unit ={
    val pq = PriorityQueue.empty[(Double, Node)](MinOrderNodeStatic)
    var s_node = hm.get(s).get

    val dist_static: Array[Double] = Array.ofDim(nodes.length)
    for (i <- 0 to dist_static.length -1){
      dist_static(i) = Double.PositiveInfinity
    }

    dist(s_node.i).lastAlpha = 1
    dist(s_node.i).lastBeta = 0
    pq.enqueue((0, s_node))
    dist_static(s_node.i) = 0

    var n: Node = null
    while (pq.nonEmpty){
      n = pq.dequeue()._2
      //println(dist_static(n.i))
      for (e <- n.edges){
        var a = dist(e.s.i).lastAlpha * e.af.getAlpha(1) //alpha_out * alpha_in
        var b = e.af.getAlpha(1) * dist(e.s.i).lastBeta + e.af.getBeta(1) // alpha_out * beta_in + beta_out
        if (dist_static(n.i) + e.af.getBeta(1) < dist_static(e.t.i)){
          dist_static(e.t.i) = dist_static(n.i) + e.af.getBeta(1)

          dist(e.t.i).lastAlpha = a
          dist(e.t.i).lastBeta = b
          println(a+" "+ b)
          if(a.isNaN || a.isInfinite){
            throw new RuntimeException("asdsa")
          }

          pq.enqueue((dist_static(e.t.i), e.t))
        }
      }
    }

  }

  /**
   * Algorithm published in paper:
   * Ding, B.; Yu, J. X. & Qin, L. Finding time-dependent shortest paths over large graphs Proceedings of the 11th international conference on Extending database technology: Advances in database technology, 2008, 205-216
   * EA_s*(*)
   * @param s
   * @param interval
   * @return
   */
  def DING(s: Int, interval: (Double, Double)): Array[ArrivalFunction] = {
    throw new Exception("implement me please!")
  }

  /**
   * Reverse Dijkstra algorithm
   * LD_*t(t) - Least Departure for one time
   * O(E + N log N)
   * @param t - target node
   * @param ta - arrival time in target node
   * @return - dist nodes map
   */
  def RDSP(t: Int, ta: Double): Array[Double] = {
    var dist: Array[Double] = Array.ofDim[Double](nodes.length)
    val pq = PriorityQueue.empty[(Double, Node)](MaxOrderNodeStatic)
    //inicialization
    for (i <- 0 to nodes.length - 1){
      dist(i) = Double.NegativeInfinity
    }
    val t_node = hm.get(t).get
    dist(t_node.i) = ta
    pq.enqueue((dist(t_node.i), t_node))
    var count = 0

    //main loop
    while (pq.nonEmpty){
      val n = pq.dequeue()._2

      for (e <- n.redges){
        val new_time = e.af.getDeparture(dist(n.i))
        if (new_time > dist(e.s.i)){
          dist(e.s.i) = new_time
          pq.enqueue((new_time, e.s))
          count += 1
        }
      }
    }
    //println("RDSP count: "+count)
    return dist
  }
}

object MinOrderNode extends Ordering[(ArrivalFunction, Node)] {
  def compare(x:(ArrivalFunction, Node), y:(ArrivalFunction, Node)) = y._1.minTravelTime compare x._1.minTravelTime
}
object MinOrderNodeStatic extends Ordering[(Double, Node)] {
  def compare(x:(Double, Node), y:(Double, Node)) = y._1 compare x._1
}
object MaxOrderNodeStatic extends Ordering[(Double, Node)] {
  def compare(x:(Double, Node), y:(Double, Node)) = x._1 compare y._1
}
object  MinOrderDEANInf extends  Ordering[(AttrNodeDEAN, Node)]{
  def compare(x: (AttrNodeDEAN,Node), y: (AttrNodeDEAN,Node)) = x._1.lastAlpha compare y._1.lastAlpha
}
object  MaxOrderEdgeZ extends  Ordering[AttrEdgeDEAN]{
  def compare(x: AttrEdgeDEAN, y: AttrEdgeDEAN) = y.Z compare x.Z
}

