import org.apache.spark.graphx.Graph

/**
 * Created by kolovsky on 14.12.15.
 */
object TestTemp {
  def main(args: Array[String]) {
    /*val o = new Operations(0.01)
    val x1 = Array(10.0,11,13,15,18,20,21)
    val y1 = Array(12.0,15,15,15,21,23,23)

    val x2 = Array(10.0,12,15,16,19,21)
    val y2 = Array(14.0,14,17,19,22,25)
    val a = new ArrivalFunction(x1, y1)
    val b = new ArrivalFunction(x2, y2)

    println(a)
    println(b)
    println(o.min(a, b))
    println(o.combination(a,b))

    print(a.getArrival(b.getArrival(10.0))+" ")
    print(a.getArrival(b.getArrival(12.0))+" ")
    print(a.getArrival(b.getArrival(13.0))+" ")
    print(a.getArrival(b.getArrival(15.0))+" ")
    print(a.getArrival(b.getArrival(15.5))+" ")
    print(a.getArrival(b.getArrival(16.0))+" ")
    print(a.getArrival(b.getArrival(17.0))+" ")
    print(a.getArrival(b.getArrival(18.0))+" ")
    print(a.getArrival(b.getArrival(19.0))+" ")
    print(a.getArrival(b.getArrival(20.333333333333332))+" ")
    print(a.getArrival(b.getArrival(21.0))+" ")*/
    val af1 = new ArrivalFunction(Array(0,5,10), Array(0, 10, 10))
    val af2 = new ArrivalFunction(Array(0,5,10), Array(5, 5, 15))
    val o = new Operations(0.01)
    val g = new Graph(Array((1,2,af1),(1,2,af2)),o)
    g.DEAN(1,(1,1))

  }
}
