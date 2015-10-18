object Both {
  def unapply[T](arg: T): Option[(T,T)] = Some(arg, arg)
}

1 match {
  case Both(x, y) => println((x, y))
}
