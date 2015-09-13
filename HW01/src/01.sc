// a dice is considered to be standing on the first tile already
val K = 3
import scala.collection.mutable.MutableList
val ways: MutableList[Int] = MutableList(1)
val N = 4
for (cell <- 2 to N) {
  ways += ways.takeRight(K).sum
}
println(ways.last)