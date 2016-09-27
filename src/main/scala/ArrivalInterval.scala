/**
 * Created by kolovsky on 9.11.15.
 */
class ArrivalInterval(x_array: Array[Double], y_array: Array[Double]) extends ArrivalFunction(x_array, y_array) with Serializable{
  override def getX(i: Int): Double = {
    return x(i)
  }
  override def getY(i: Int): Double = {
    return y(i)
  }

  def f(xa: Double): Double = {
    if(xa < this.x(0) || xa > this.x.last){
      throw new RuntimeException("X out of bound: "+ xa)
    }
    var x = xa

    if(x == this.x.last){
      return this.y.last
    }

    var upper: Int = this.x.length - 1
    var lower: Int = 1
    var count  = 0
    while (true){
      var i: Int = lower + (upper - lower) / 2
      if (x >= this.x(i - 1) && x < this.x(i)){
        val ret = this.y(i - 1) + ((this.y(i) - this.y(i - 1))/(this.x(i) - this.x(i - 1))) * (x - this.x(i - 1))
        if (ret.isNaN || ret.isInfinite){
          throw new RuntimeException("Y is Nan or Infinyty")
        }
        return ret
      }
      if (x < this.x(i - 1)){
        upper = i - 1
      }
      if (x >= this.x(i)){
        lower = i + 1
      }
      if(count > this.x.length){
        throw new RuntimeException("zacikleni (f): lower: "+this.x(lower)+" upper: "+ this.x(upper)+" y: "+x)
      }
      count += 1
    }
    return 0

  }
  def fI(ya: Double): Double = {
    if (ya < this.y(0) || ya > this.y.last){
      return -1
    }
    var y = ya

    if(y == this.y.last){
      return this.x.last
    }

    var upper: Int = this.y.length - 1
    var lower: Int = 1

    var count = 0
    while (true){
      var i: Int = lower + (upper - lower) / 2
      if (y >= this.y(i - 1) && y < this.y(i)){
        var ret =  this.x(i - 1) + ((this.x(i) - this.x(i - 1))/(this.y(i) - this.y(i - 1))) * (y - this.y(i - 1))
        return ret
      }
      if (y < this.y(i - 1)){
        upper = i - 1
      }
      if (y >= this.y(i)){
        lower = i + 1
      }
      if(count > this.x.length + 10){
        return this.x(lower)
        throw new RuntimeException("zacikleni(fI): lower: "+this.y(lower)+" upper: "+ this.y(upper)+" y: "+y)
      }
      count += 1
    }
    return 0

  }

}
