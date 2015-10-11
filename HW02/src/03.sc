import scala.collection.immutable.Stream
import scala.math.BigInt
def ninesFrom(n: BigInt): Stream[BigInt] = n #:: ninesFrom( n * 10 + 9)
val numbers = (2 until 1000).filter( n => (n % 2 != 0) && (n % 5 != 0))
val periods = numbers.map { n => ninesFrom(9)
  .find( nineNum => (nineNum % n) == 0)
  .map( nineNum => nineNum.toString().length)}
val result = numbers.zip (periods).maxBy { case (num, per) => per.getOrElse(0)}