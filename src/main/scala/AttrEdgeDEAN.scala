import scala.collection.mutable.ListBuffer

/**
 * Created by kolovsky on 7.1.16.
 */
class AttrEdgeDEAN(edge: Edge) extends Serializable{
  val e: Edge = edge
  var X: Double = 0
  var Y: Double = 0
  var Z: Double = 0
  var i: Int = 1

  var alpha_i: Double = 0
  var beta_i: Double = 0
  // for target node (j)
  var alpha_j:Double = 0
  var beta_j: Double = 0
  // for edge
  var alpha_ij: Double = 0
  var beta_ij: Double = 0
  var tau_ij: Double = 0

  def computeXYZ(dist: Array[AttrNodeDEAN]): Unit ={
    // for source node (i)
    alpha_i = dist(e.s.i).lastAlpha
    beta_i = dist(e.s.i).lastBeta
    // for target node (j)
    alpha_j = dist(e.t.i).lastAlpha
    beta_j = dist(e.t.i).lastBeta
    // for edge
    alpha_ij = e.af.getAlpha(i)
    beta_ij = e.af.getBeta(i)
    tau_ij = e.af.getTau(i)._2

    println("alpha_i = "+alpha_i)
    println("beta_i = "+beta_i)
    println("alpha_j = "+alpha_j)
    println("beta_j = "+beta_j)
    println("alpha_ij = "+alpha_ij)
    println("beta_ij = "+beta_ij)

    X = (tau_ij - beta_i) / alpha_i

    if (alpha_ij * alpha_i != alpha_j){
      Y = (beta_j - beta_ij - alpha_ij * beta_i) / (alpha_ij * alpha_i - alpha_j)
    }
    else{
      Y = Double.PositiveInfinity
    }

    if (X.isNaN || Y.isNaN){
      println("alpha_i = "+alpha_i)
      println("beta_i = "+beta_i)
      println("alpha_j = "+alpha_j)
      println("beta_j = "+beta_j)
      println("alpha_ij = "+alpha_ij)
      println("beta_ij = "+beta_ij)
      println(beta_j - beta_ij - alpha_ij * beta_i)
      println((alpha_ij * alpha_i - alpha_j))
      throw new RuntimeException("X, Y: "+X + " " + Y)

    }

    Z = math.min(X, Y)
    println("Y: "+Y)
    println("X: "+X)
    println("Z: "+Z)
  }
  def move(): Unit ={
    i += 1
  }
}
