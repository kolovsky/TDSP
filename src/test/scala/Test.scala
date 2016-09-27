import java.io.{FileOutputStream, ObjectOutputStream}

import scala.collection.mutable.{ListBuffer, HashMap}
import scala.io.Source

/**
 * Created by kolovsky on 10.11.15.
 */
object Test {
  def main(args: Array[String]) {
    var edges: List[(Int, Int, ArrivalFunction)]  = List()
    var lines = Source.fromFile("/home/kolovsky/Dokumenty/skola/BP/agile/datasets/road_1000_v4").getLines()

    for(line <- lines){
      var row = line.split(";")
      var af: ArrivalFunction = new ArrivalFunction(row(1).split(",").map(_.toDouble), row(2).split(",").map(_.toDouble))
      edges = edges :+ (row(4).toInt, row(5).toInt, af)
      edges = edges :+ (row(5).toInt, row(4).toInt, af)

    }
    var a = System.currentTimeMillis()
    val o =new Operations(0.01)
    val g: Graph = new Graph(edges.toArray, o)
    //val obj = new ObjectOutputStream(new FileOutputStream("/tmp/pokus.dat"))
    //obj.writeObject()
    //obj.close()

    var krok = 4300
    var up = krok
    var down = 0
    var out: ListBuffer[(Int ,ArrivalFunction)] = ListBuffer()
    while(up <= 60000){
      var dist_map = g.LCA(27931, (down, up))
      //throw new Exception("s")
      //print("a")

      for (i <- 0 to dist_map.length - 1){
        out.+=((i, dist_map(i)))
      }

      down = up
      up += krok
    }
    val out2 = out.toArray
    val results = out2.groupBy(_._1).map( x => (x._1, x._2.map(_._2)) ).map(x => (x._1,o.merge(x._2))).toArray
    println("\n"+results(10)._2.x.length)
    println((System.currentTimeMillis-a)/1000.0)

    a = System.currentTimeMillis()
    g.DSP(27931,0)
    println((System.currentTimeMillis-a)/1000.0)

    a = System.currentTimeMillis()
    g.RDSP(27931, 80000)
    println((System.currentTimeMillis-a)/1000.0)

    a = System.currentTimeMillis()
    //g.DEHNE(27931,10,(0,0))
    println((System.currentTimeMillis-a)/1000.0)
  }
}
