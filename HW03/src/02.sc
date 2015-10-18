class A(val foo: Int)

class B(val bar: Int)
object B {
  implicit def a2b(a:A): B = new B(a.foo)
}

class C(val baz: Int)
object C {
  implicit def b2c[T](b: T)(implicit ev: T => B): C = new C(b.bar)
}

val c: C = new A(2)
c.baz