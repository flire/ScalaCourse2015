import scala.collection.mutable.ArrayBuffer

class Node(val id: String)
class Edge[+N <: Node](val left: N, val right: N)

trait Graph[N <: Node, E <: Edge[N]] {
  class Edge(val left: N, val right: N)
  protected val nodes: ArrayBuffer[N] = ???
  protected var edges: ArrayBuffer[E] = ???

  def connect(edge: E) = {
    if (isConnectionPossible(edge.left, edge.right)) {
      addEdge(edge)
    }
  }

  protected def addEdge(edge: E)
  def isConnectionPossible(a: N, b: N): Boolean
}