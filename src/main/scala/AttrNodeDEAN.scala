import scala.collection.mutable.ListBuffer

/**
 * Created by kolovsky on 10.2.16.
 */
class AttrNodeDEAN() extends Serializable{
  val breakPoints: ListBuffer[(Double, Double, Double)] = ListBuffer()
  var lastAlpha: Double = Double.PositiveInfinity
  var lastBeta: Double = 0

  def recordCurrentPiece(e: AttrEdgeDEAN, t_new: Double): Unit = {
    breakPoints += ((lastAlpha, lastBeta, t_new))
    println("("+lastAlpha+", "+lastBeta+", "+t_new+")")

    lastAlpha = e.alpha_ij * e.alpha_j
    lastBeta = e.alpha_ij * e.beta_i + e.beta_ij
    println(lastAlpha +" "+lastBeta)
  }
  def getArrivalFunction(): ArrivalFunction ={
    //println(breakPoints)
    var x: Array[Double] = Array.ofDim(breakPoints.length)
    var y: Array[Double] = Array.ofDim(breakPoints.length)
    //x(0) = 0
    //y(0) = breakPoints(0)._2
    var i = 0
    for (bp <- breakPoints){
      x(i) = bp._3
      y(i) = bp._1 * bp._3 + bp._2
      i += 1
    }
    return new ArrivalFunction(x,y)
  }
}
