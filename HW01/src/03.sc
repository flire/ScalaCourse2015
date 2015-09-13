import Math.sqrt
val vertices = List((0d,0d), (0d,1d), (1d,1d), (1d,0d))
def perimeter(vertices: List[(Double, Double)]): Double = {
  val shiftedVertices = vertices.drop(1) :+ vertices(0)
  vertices.zip(shiftedVertices).map(Function.tupled(distance)).sum
}

def distance(dot1: (Double, Double), dot2: (Double, Double)): Double = {
  val (x1, y1) = dot1
  val (x2, y2) = dot2
  sqrt(square(x1 - x2) + square(y1-y2))
}

def square(x: Double) = x * x
def area(vertices: List[(Double, Double)]) = {
  val trisectionPoint = vertices(0)
  val remaining = vertices.drop(1)
  val edges = remaining.dropRight(1).zip(remaining.drop(1))
  val areaCalcFunction: ((Double, Double), (Double, Double)) => Double =
    triangleArea(trisectionPoint, _, _)
  edges.map(Function.tupled(areaCalcFunction)).sum
}

//calculated with Heron's formula
def triangleArea(dot1: (Double, Double),
                 dot2: (Double, Double),
                 dot3: (Double, Double)): Double = {
  val s = perimeter(List(dot1, dot2, dot3)) / 2d
  val a = distance(dot1, dot2)
  val b = distance(dot2, dot3)
  val c = distance(dot3, dot1)
  sqrt(s * (s - a) * (s - b) * (s - c))
}

println(s"perimeter: ${perimeter(vertices)}")
println(s"area: ${area(vertices)}")