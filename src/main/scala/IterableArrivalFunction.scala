/**
 * Created by kolovsky on 15.12.15.
 */
class IterableArrivalFunction(afin: ArrivalFunction){
  val af = afin
  var actualIndex = 0

  def y(): Double ={
    return this.af.y(actualIndex)
  }
  def x(): Double = {
    return this.af.x(actualIndex)
  }
  def moveActualIndex(): Unit ={
    actualIndex += 1
  }
}
