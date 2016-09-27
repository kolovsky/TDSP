/**
 * Created by kolovsky on 9.11.15.
 */
class ArrivalFunction(x_array: Array[Double], y_array: Array[Double]) extends Serializable{
  //X - part of breakpoint
  val x = x_array
  //Y - part of breakpoint
  val y = y_array
  //check import array
  checkInputArray()
  //minimum travel time
  val minTravelTime = computeMinMaxTravelTime()._1
  //maximum travel time
  val maxTravelTime = computeMinMaxTravelTime()._2

  /**
   * Compute minimum and maximum travel time
   * @return (minTravelTime, maxTravel Time)
   */
  def computeMinMaxTravelTime():(Double, Double) = {
    var min = Double.PositiveInfinity
    var max = Double.NegativeInfinity
    if (x.length != 0){
      for (i <- 0 to x.length - 1){
        if (y(i) - x(i) < min){
          min = y(i) - x(i)
        }
        if (y(i) - x(i) > max){
          max = y(i) - x(i)
        }
      }
    }
    return (min,max)
  }

  /**
   * Control arrival function properties (e.g FIFO)
   */
  def checkInputArray(): Unit = {
    if (x.length != y.length){
      throw new IllegalArgumentException("x and y must have same length")
    }
    for (i <- 1 to x.length - 1){
      if (y(i) - y(i - 1) < 0){
        throw new IllegalArgumentException("input function is not FIFO")
      }
      if (x(i) - x(i - 1) <= 0){
        throw new IllegalArgumentException("departure time is not sorted")
      }
    }
  }

  /**
   * Infinity function (undefined)
   * @return
   */
  def isInfinity():Boolean = {
    if (minTravelTime.isPosInfinity){
      return true
    }
    return false
  }

  /**
   * Function y = alpha(tau) + beta
   * @param i line segment index
   * @return alpha
   */
  def getAlpha(i: Int): Double = {
    return (y(i) - y(i-1)) / (x(i) - x(i-1))
  }

  /**
   * Function y = alpha(tau) + beta
   * @param i line segment index
   * @return beta
   */
  def getBeta(i: Int): Double = {
    return y(i-1) - x(i-1) * getAlpha(i)
  }

  /**
   *
   * @param i line segment index
   * @return bound (x(i-1),x(i))
   */
  def getTau(i: Int): (Double, Double) = {
    return (x(i - 1), x(i))
  }

  /**
   * This function use binary search O(n log n)
   * @param X departure time
   * @param si segment index (from 1 to length) (use if you know segment where input time lie) O(1)
   * @return arrival time
   */
  def getArrival(X: Double, si:Int = 0): Double = {
    if (si == 0){
      val v = checkInputDeparture(X)
      val Xr = v._1
      val n = v._2
      val i = binarySearch(Xr, this.x)
      return getAlpha(i) * Xr + getBeta(i) + n * (x.last - x(0))
    }
    else{
      val ir = checkInputSegmentIndex(si)._1
      val v = checkInputDeparture(X)
      val Xr = v._1
      val n = v._2
      if (Xr >= x(ir - 1) && Xr < x(ir)){
        return getAlpha(ir) * Xr + getBeta(ir) + n * (x.last - x(0))
      }
      else{
        throw new IllegalArgumentException("X is not in segment with index "+si)
      }
    }

  }

  /**
   * This function use binary search (n log n)
   * @param Y - arrival time
   * @param si segment index (from 1 to length) (use if you know segment where input time lie) O(1)
   * @return departure time
   */
  def getDeparture(Y: Double, si:Int = 0): Double = {
    if (si == 0){
      val v = checkInputArrival(Y)
      val Yr = v._1
      val n = v._2
      val i = binarySearch(Yr, this.y)
      return (Yr - getBeta(i)) / getAlpha(i) + n * (y.last - y(0))
    }
    else{
      if (Y >= y(si - 1) && Y < y(si)){
        return (Y - getBeta(si)) / getAlpha(si)
      }
      else{
        throw new IllegalArgumentException("X is not in segment with index "+si)
      }
    }
  }

  /**
   * This function control input departure time.
   * Correct interval is [x(0),x.last)
   * @param X - input departure time
   * @return (Xr, n) Xr - X transform to interval, n - move
   */
  def checkInputDeparture(X: Double): (Double, Int) = {
    var n = 0
    var Xr = X
    var dx = x.last - x(0)
    if (X < x(0)){
      Xr = x.last - (x.last - X) % dx
      n = - ((x.last - X) / dx).toInt
    }
    if (X >= x.last){
      Xr = x(0) + (X - x(0)) % dx
      n = ((X - x(0)) / dx).toInt
    }
    return (Xr, n)
  }

  /**
   * This function control input arrival time.
   * Correct interval is [y(0),y.last)
   * @param Y - input arrival time
   * @return (Yr,n) Yr - Y transform on interval, n - move
   */
  def checkInputArrival(Y: Double): (Double, Int) = {
    var n = 0
    var Yr = Y
    var dy = y.last - y(0)
    if (Y < y(0)){
      Yr = y.last - (y.last - Y) % dy
      n = - ((y.last - Y) / dy).toInt
    }
    if (Y >= y.last){
      Yr = y(0) + (Y - y(0)) % dy
      n = ((Y - x(0)) / dy).toInt
    }
    return (Yr, n)
  }

  /**
   * This function reduce segment index on right interval
   * @param i - segment index
   * @return (reduced SI, n)
   */
  def checkInputSegmentIndex(i: Int): (Int, Int) ={
    val size: Int = x.length - 1
    var ir = i
    var n = 0
    if(i > size){
      ir = i % size
      n = i / size
    }
    if(i < 1){
      throw new Exception("segment index must be > 1: "+i)
    }
    return (ir, n)
  }

  /**
   * line segment 'i' is interval [bp(i-1),bp(i))
   * @param value finding value
   * @param bp array of values
   * @return line segment index
   */
  def binarySearch(value: Double, bp: Array[Double]): Int ={
    var up: Int  = bp.length - 1
    var lo: Int = 1
    var i = -1
    if(value >= bp(0) && value < bp.last){
      var j = 0
      while(j < bp.length){
        i = lo + (up - lo) / 2
        if(value >= bp(i - 1) && value < bp(i)){
          return i
        }
        if (value < bp(i - 1)){
          up = i - 1
        }
        if (value >= bp(i)){
          lo = i + 1
        }
        j += 1
      }
    }
    else {
      throw new RuntimeException("input value out off interval: value: "+value+" bp(0): "+bp(0)+" bp.last: "+bp.last)
    }

    throw new RuntimeException("problem in binary search (infinity loop): value: "+value+" bp: "+bp.toString)
  }

  /**
   *
   * @return number of breakpoints
   */
  def length(): Int ={
    return x.length
  }

  /**
   * Copy arrival functon
   * @return arrival function
   */
  def copy(): ArrivalFunction = {
    return new ArrivalFunction(this.x.clone(), this.y.clone())
  }

  /**
   *
   * @param i - breakpoint index
   * @return departure time on breakpoint index (from 0 to ...)
   */
  def getX(i: Int): Double = {
    if (i == 0){
      return x(0)
    }
    val ch = checkInputSegmentIndex(i)
    return this.x(ch._1) + ch._2 * (this.x.last - this.x(0))
  }
  /**
   *
   * @param i - breakpoint index
   * @return arrival time on breakpoint index (from 0 to ...)
   */
  def getY(i: Int): Double = {
    if (i == 0){
      return y(0)
    }
    val ch = checkInputSegmentIndex(i)
    return this.y(ch._1) + ch._2 * (this.x.last - this.x(0))
  }


  def generalizate(lambda: Double): ArrivalFunction = {
    if (lambda == 0){
      return this
    }
    val len = this.x.length
    def g(lower: Int, upper: Int): List[Int] = {
      if (upper - lower == 1){
        return List()
      }
      var alpha_max: Double = 0
      var alpha_max_i = 0
      for (point <- lower + 1 to upper - 1){
        var alpha = y(point) - (((y(upper) - y(lower)) / (x(upper) - x(lower))) * (x(point) - x(lower)) + y(lower))

        alpha = math.abs(alpha)
        if (alpha > alpha_max && alpha > lambda){
          alpha_max = alpha
          alpha_max_i = point
        }
      }
      if (alpha_max == 0){
        return List()
      }
      return (g(lower, alpha_max_i) :+ alpha_max_i) ++ g(alpha_max_i, upper)
    }

    val indexes = (0 +: g(0, x.length - 1) :+ x.length - 1).toArray
    var lx:List[Double] = List()
    var ly:List[Double] = List()
    for (i <- indexes.sorted){
      lx = lx :+ x(i)
      ly = ly :+ y(i)
    }

    return new ArrivalFunction(lx.toArray, ly.toArray)

  }

  /**
   *
   * @return return function in Octave format
   */
  override def toString(): String = {
    var x:String = ""
    var y:String = ""
    for( point <- 0 to this.x.length - 1) {
      x += " " + this.x(point)
      //y += " " + (this.y(point) - this.x(point))
      y += " " + (this.y(point))

    }
    return "x = [" + x + "]\ny = [" + y + "]"
  }
}
