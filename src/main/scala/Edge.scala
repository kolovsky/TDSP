/**
 * Created by kolovsky on 9.11.15.
 */
class Edge(source: Node, target: Node, arrival_function: ArrivalFunction) extends Serializable{
  val s = source
  val t = target
  var af = arrival_function
  var i: Int = -1
  var id: Int = -1
}
