//heterougenous list with append

sealed trait HList

case class HCons[+H, +T <: HList](head: H, tail: T) extends HList

case object HNil extends HList
type HNil = HNil.type


sealed trait NumeralIndex
case class Succ[+P <: NumeralIndex ](prev: P) extends NumeralIndex
case object Zero extends NumeralIndex
type Zero = Zero.type
case class SplitResult[L, R <: HList](left: L, right: R)

trait Splittable[L <: HList, I <: NumeralIndex, ResLeft <: HList, ResRight <: HList] {
  def apply(list: L, index: I): SplitResult[ResLeft, ResRight]
}

object Splittable {
  implicit def splitPoint[L <: HList]: Splittable[L, Zero, HNil, L] = {
    new Splittable[L, Zero, HNil, L] {
      override def apply(list: L, index: Zero): SplitResult[HNil, L] = SplitResult(HNil, list)
    }
  }

  implicit def splitLater[L <: HList, I <: NumeralIndex, H, ResLeft <: HList, ResRight <: HList]
  (implicit a: Splittable[L, I, ResLeft, ResRight]):
  Splittable[HCons[H, L], Succ[I], HCons[H, ResLeft], ResRight] = {
    new Splittable[HCons[H, L], Succ[I], HCons[H, ResLeft], ResRight] {
      override def apply(list: HCons[H, L], index: Succ[I]): SplitResult[HCons[H, ResLeft], ResRight] = {
        val splitResult = a(list.tail, index.prev)
        SplitResult(HCons(list.head, splitResult.left), splitResult.right)
      }
    }
  }
}

def splitAt[L <: HList, I <: NumeralIndex, ResLeft <: HList, ResRight <: HList](list: L, index: I)(
  implicit splittable: Splittable[L, I, ResLeft, ResRight]
  ): SplitResult[ResLeft, ResRight] = splittable(list, index)
splitAt(HCons("", HCons(1, HCons(false, HNil))), Succ(Succ(Zero))).right.head