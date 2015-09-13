import scala.annotation.tailrec
import math.min
import math.max

def egcd(a: Int, b: Int): (Int, Int, Int) = {
  val (gcd, (x, y)) = egcd_helper(min(a, b), max(a, b), List())
  if ( a > b ) (gcd, y, x) else (gcd, x, y)
}
@tailrec
def egcd_helper(a: Int, b: Int, list: List[Int]): (Int, (Int, Int)) = {
  if (a == 0) (b, list.foldLeft( (0,1) ) { (coeffs: (Int, Int), mult: Int) =>
    val (y, x) = coeffs
    ( x - mult * y, y)
  })
    else egcd_helper( b % a, a, (b / a) :: list)
}
val a = 30
val b = 24
val (gcd, x, y) = egcd(a,b)
println(s"gcd = $gcd x = $x y = $y")//where gcd == a*x + b*y
