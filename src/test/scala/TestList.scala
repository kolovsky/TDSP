import scala.collection.mutable

/**
 * Created by kolovsky on 22.11.15.
 */
object TestList {
  def main(args: Array[String]) {

    println("append")
    println("======================")

    var a = System.currentTimeMillis()
    var out = List[Int]()
    for (i <- 0 to 10000){
      out = out :+ i
    }
    out.toArray
    println("List: "+(System.currentTimeMillis() - a))

    a = System.currentTimeMillis()
    var out2 = Array[Int]()
    for (i <- 0 to 10000){
      out2 = out2 :+ i
    }
    println("Array (arr = arr :+ item): "+(System.currentTimeMillis() - a))

    a = System.currentTimeMillis()
    var out3 = Array.ofDim[Int](10001)
    for (i <- 0 to 10000){
      out3(i) = i
    }
    println("Array (arr(i) = item): "+(System.currentTimeMillis() - a))

    a = System.currentTimeMillis()
    var out4 = mutable.MutableList[Int]()
    for (i <- 0 to 10000){
      out4 += i
    }
    out4.toArray
    println("MutableList: "+(System.currentTimeMillis() - a))

    a = System.currentTimeMillis()
    var out5 = mutable.ListBuffer[Int]()
    for (i <- 0 to 10000){
      out5 += i
    }
    out5.toArray
    println("ListBuffer: "+(System.currentTimeMillis() - a))

    a = System.currentTimeMillis()
    var out6 = mutable.ArrayBuffer[Int]()
    for (i <- 0 to 10000){
      out6 += i
    }
    out6.toArray
    println("ArrayBuffer: "+(System.currentTimeMillis() - a))

    println("iterace")
    println("======================")//===============================================================================

    a = System.currentTimeMillis()
    for (i <- out){
      i
    }
    println("List: "+(System.currentTimeMillis() - a))

    a = System.currentTimeMillis()
    for (i <- out2){
      i
    }
    println("Array (i <- out): "+(System.currentTimeMillis() - a))

    a = System.currentTimeMillis()
    for (i <- 0 to 10000){
      out3(i)
    }
    println("Array (arr(i) = item): "+(System.currentTimeMillis() - a))

    a = System.currentTimeMillis()

    for (i <- out4){
      i
    }

    println("MutableList: "+(System.currentTimeMillis() - a))

    a = System.currentTimeMillis()
    for (i <- out5){
      i
    }

    println("ListBuffer: "+(System.currentTimeMillis() - a))

    a = System.currentTimeMillis()
    for (i <- out6){
      i
    }

    println("ArrayBuffer: "+(System.currentTimeMillis() - a))


  }



}
