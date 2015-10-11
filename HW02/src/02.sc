object A {
  def foo(): Int = 1
}
trait A {
  def goo(): Int
}
class B extends A {
  def foo(): Int = A.foo()
  def goo(): Int = 2
}
class C$ {
  def m(): Int = 1
}
object C$ {
  val MODULE$ = new C$()
}
final class C {
  def m() = C$.MODULE$.m()
}
