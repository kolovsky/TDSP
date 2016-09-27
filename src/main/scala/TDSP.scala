import org.apache.spark.{SparkContext, SparkConf}

import scala.collection.mutable.ListBuffer
import scala.io.Source

/**
 * Created by kolovsky on 13.11.15.
 */
object TDSP {
  def main(args: Array[String]) {
    val conf = new SparkConf()

    val sc = new SparkContext(conf)

    val filename = args(0)
    val number_of_intervals = args(1).toInt
    val number_of_partitions = args(2).toInt
    val maxError: Double = args(3).toDouble

    val o = sc.broadcast(new Operations(maxError))

    var edges: List[(Int, Int, ArrivalFunction)]  = List()
    var lines = Source.fromFile(args(0)).getLines()
    for(line <- lines){
      var row = line.split(";")
      var af: ArrivalFunction = new ArrivalFunction(row(1).split(",").map(_.toDouble), row(2).split(",").map(_.toDouble))
      edges = edges :+ (row(4).toInt, row(5).toInt, af)
      edges = edges :+ (row(5).toInt, row(4).toInt, af)

    }
    val g = sc.broadcast(edges.toArray)
    var parallel_array = ListBuffer[(Double, Double)]()
    var krok:Double = 86000.0/number_of_intervals
    var up: Double = krok
    var down:Double = 0
    while(up <= 86000){
      parallel_array.+=((down, up))
      down = up
      up += krok
    }
    println("dekla: "+parallel_array.length)

    def computeSearch(bound: (Double,Double)): Array[ArrivalInterval] = {
      val graph  = new Graph(g.value, o.value)
      return graph.dijkstra(27931, bound)
    }

    var a = System.currentTimeMillis()

    val graphs = sc.parallelize(parallel_array, number_of_partitions)
    val distmap = graphs.map(computeSearch)

    def distMap_to_interval(x: Array[ArrivalInterval]): ListBuffer[(Int, ArrivalInterval)] = {
      var out: ListBuffer[(Int,ArrivalInterval)] = ListBuffer()
      for (i <- 0 to x.length - 1){
        out.+=((i,x(i)))
      }
      return out
    }
    val all_interval = distmap.flatMap(distMap_to_interval)
    val ai_in_node =  all_interval.groupBy(_._1)
    /**def marge_intervals(in: (Int, Iterable[(Int,ArrivalInterval)])): (Int, ArrivalInterval) ={
      val intervals = in._2.map(_._2).toArray
      //val ai = o.value.merge(intervals)
      val ai:Array[ArrivalFunction] = Array()
      return (in._1, ai)
    }
    val results = ai_in_node.map(marge_intervals)
    println(results.collect()(0)._2.x.length)
    println("time: "+(System.currentTimeMillis() - a))*/
  }
}
