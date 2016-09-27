import scala.collection.mutable.ListBuffer

/**
 * Created by kolovsky on 19.9.15.
 */
class Operations(maxError: Double) extends Serializable{
  // maximal error for (1 +- e) - aproximation
  val this.maxError = maxError

  /**
   * Marge arrival functions by time
   * @param in - list of func.
   * @return arrival functions
   */
  def merge(in: Array[ArrivalFunction]): ArrivalFunction = {
    var nx: Array[Double] = Array()
    var ny: Array[Double] = Array()
    var functions: ListBuffer[ArrivalFunction] = ListBuffer()
    for (f <- in){
      if (!f.isInfinity()){
        functions += f
      }
    }
    for(f <- functions.sortBy(_.x(0))){
      nx = nx ++ f.x.clone.slice(0,f.x.length - 1)
      ny = ny ++ f.y.clone.slice(0,f.y.length - 1)
    }
    return new ArrivalInterval(nx, ny)

  }

  /**
   * Compure out(in(t)) O(N_out + N_in)
   * @param out
   * @param in
   * @return out(in(t))
   */
  def combination(out: ArrivalFunction, in: ArrivalFunction): ArrivalFunction = {
    if(out.isInfinity() || in.isInfinity()){
      return new ArrivalFunction(Array(), Array())
    }
    val redLo = out.checkInputDeparture(in.y(0))

    val outLoIndex = out.binarySearch(redLo._1, out.x) + redLo._2 * (out.length - 1)


    // list for output arrival function
    val xo: ListBuffer[Double] = ListBuffer()
    val yo: ListBuffer[Double] = ListBuffer()

    def addBreakPoint(x:Double, y:Double): Unit ={
      if(!xo.isEmpty){
        if (x <= xo.last){
          throw new IllegalArgumentException("cannot add breakpoint: x <= xo.last")
        }
        if (y < yo.last){
          throw new IllegalArgumentException("cannot add breakpoint: y < yo.last")
        }
      }
      xo += x
      yo += y
    }

    var i: Int = 0 //in function
    var j: Int = outLoIndex //out function

    while (i < in.length){
      if (in.y(i) == out.getX(j)){
        addBreakPoint(in.x(i), out.getY(j))
        i += 1
        j += 1
      }
      else if (in.y(i) < out.getX(j)){
        addBreakPoint(in.x(i), out.getArrival(in.y(i), j))
        i += 1

      }
      else{
        addBreakPoint(in.getDeparture(out.getX(j), i), out.getY(j))

        j += 1
      }
    }
    if (in.x(0) != xo(0) || in.x.last != xo.last){
      throw new RuntimeException("bad out")
    }
    return (new ArrivalFunction(xo.toArray, yo.toArray)).generalizate(maxError)

  }

  /**
   * Compure minimum from two arrival function (minimum arrival time) O(n1 + n2)
   * @param af1 - first arrival function
   * @param af2 - second arrival function
   * @return minimum from input function
   */
  def min(af1: ArrivalFunction, af2: ArrivalFunction): ArrivalFunction = {
    if (af1.isInfinity()){
      return af2.copy()
    }
    if (af2.isInfinity()){
      return af1.copy()
    }
    if (af1.x(0) != af2.x(0) || af1.x.last != af2.x.last){
      val es:String = " ("+af1.x(0)+" "+af1.x.last+") ("+af2.x(0)+" "+af2.x.last+")"
      throw new IndexOutOfBoundsException("input arrival functions have different definition interval:"+es)
    }
    if(af1.minTravelTime > af2.maxTravelTime){
      return af2.copy()
    }
    if(af2.minTravelTime > af1.maxTravelTime){
      return af1.copy()
    }

    var afa1: IterableArrivalFunction = new IterableArrivalFunction(af1)
    var afa2: IterableArrivalFunction = new IterableArrivalFunction(af2)
    var lo: IterableArrivalFunction = null
    if (af1.y(0) < af2.y(0)){
      lo = afa1
    }
    else if (af1.y(0) > af2.y(0)){
      lo = afa2
    }

    // list for output arrival function
    val xo: ListBuffer[Double] = ListBuffer()
    val yo: ListBuffer[Double] = ListBuffer()

    def chackChange(less: IterableArrivalFunction, more: IterableArrivalFunction, more_y: Double): Unit ={
      if (less.y() == more_y){
        addBreakPoint(less.x(), less.y())
        lo = null
      }
      else if(less.y() < more_y){
        if (lo == more){
          val point = intersectionSegment(less, more)
          addBreakPoint(point._1, point._2)
          lo = less
        }
        addBreakPoint(less.x(),less.y())

      }
      else{
        if (lo == less){
          val point = intersectionSegment(less, more)
          addBreakPoint(point._1, point._2)
          lo = more
        }

      }
    }

    def addBreakPoint(x:Double, y:Double): Unit ={
      if(xo.nonEmpty){
        if (x <= xo.last){
          throw new IllegalArgumentException("cannot add breakpoint: x <= xo.last")
        }
        if (y < yo.last){
          throw new IllegalArgumentException("cannot add breakpoint: y <= yo.last")
        }
      }
      xo += x
      yo += y
    }

    while (afa1.actualIndex < afa1.af.length && afa2.actualIndex < afa2.af.length){
      if(afa1.x == afa2.x){
        if(afa1.y < afa2.y){
          chackChange(afa1, afa2, afa2.y())
        }
        else{
          chackChange(afa2, afa1, afa1.y())
        }

        afa1.moveActualIndex()
        afa2.moveActualIndex()
      }
      else if(afa1.x < afa2.x){
        chackChange(afa1, afa2, afa2.af.getArrival(afa1.x(),afa2.actualIndex))
        afa1.moveActualIndex()
      }
      else{
        chackChange(afa2, afa1, afa1.af.getArrival(afa2.x(),afa1.actualIndex))
        afa2.moveActualIndex()
      }
    }
    if (af1.x(0) != xo(0) || af1.x.last != xo.last){
      println(af1)
      println(af2)
      val es:String = " ("+af1.x(0)+" "+af1.x.last+") ("+af2.x(0)+" "+af2.x.last+")"
      throw new RuntimeException("bad out: "+es)
    }
    return new ArrivalFunction(xo.toArray, yo.toArray)
  }

  /**
   * Compare two arrival function
   * first and last breakpoint must have same departure time (functions must be defined on same interval)
   * @param n new arrival function
   * @param o old arrival function
   * @return true if part new arrival function is under old function else false
   */
  def equals(n: ArrivalFunction, o: ArrivalFunction): Boolean = {
    // new is infinyty or new is over old
    if (n.isInfinity()){
      return false
    }
    // old is infitity or minimum travel time of new is under min. travel time old
    if (o.isInfinity()){
      return true
    }
    if(n.minTravelTime > o.maxTravelTime){
      return false
    }
    if(n.minTravelTime < o.minTravelTime){
      return true
    }
    if (n.x(0) != o.x(0) || n.x.last != o.x.last){
      throw new IndexOutOfBoundsException("input arrival functions have different definition interval")
    }
    var i = 0 //index for n (new function)
    var j = 0 //index for o (old function)

    while (i < n.length && j < o.length){
      if (n.x(i) == o.x(j)){
        if (n.y(i) < o.y(j)){
          return true
        }
        i += 1
        j += 1
      }
      else if (n.x(i) < o.x(j)){
        if (n.y(i) < o.getArrival(n.x(i), j)){
          return true
        }
        i += 1
      }
      else if (n.x(i) > o.x(j)){
        if ( n.getArrival(o.x(j), i) < o.y(j)){
          return true
        }
        j += 1
      }
    }
    return false
  }

  /**
   * Find intersection of linear segment
   * @param afa1 - first iterable arrival function
   * @param afa2 - second iterable arrival function
   * @return (departureTime, arrivalTime)
   */
  def intersectionSegment(afa1: IterableArrivalFunction, afa2: IterableArrivalFunction): (Double, Double) = {
    val af1 = afa1.af
    val af2 = afa2.af
    val i = afa1.actualIndex
    val j = afa2.actualIndex

    val lo = math.max(af1.x(i - 1), af2.x(j - 1))
    val up = math.min(af1.x(i), af2.x(j))
    if (lo >= up){
      throw new Exception("departure interval not intersect: ("+af1.x(i - 1)+" "+af1.x(i)+") ("+af2.x(j - 1)+" "+af2.x(j)+")")
    }
    if(af2.getAlpha(j) == af1.getAlpha(i)){
      throw new Exception("intersect eqution do not solve (alpha1 = alpha2), line is parallel")
    }
    val x = (af1.getBeta(i) - af2.getBeta(j)) / (af2.getAlpha(j) - af1.getAlpha(i))
    if (x >= lo && x < up){
      val y = af1.getArrival(x, i)
      return (x, y)
    }
    else{
      throw new Exception("intersect is out of bound")
    }

  }

}

