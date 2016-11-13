import cats._
import shapeless._
import shapeless.labelled._
import shapeless.syntax.singleton._
object MonoidInstances {
  import cats.kernel.instances.all._

  implicit val hNilMonoid: Monoid[HNil] = new Monoid[HNil] {
    override def empty: HNil = HNil

    override def combine(x: HNil, y: HNil): HNil = HNil
  }

  implicit def hConsMonoid[H, T <: HList](implicit
    headMonoid: Monoid[H],
    tailMonoid: Lazy[Monoid[T]]) =
    new Monoid[H :: T] {
      override def empty: ::[H, T] =
        headMonoid.empty :: tailMonoid.value.empty

      override def combine(x: ::[H, T], y: ::[H, T]): ::[H, T] =
        headMonoid.combine(x.head, y.head) :: tailMonoid.value.combine(x.tail, y.tail)
    }

  implicit def caseClassMonoid[T, Repr](implicit
    gen: Generic.Aux[T, Repr],
    rMonoid: Lazy[Monoid[Repr]],
    tpe: Typeable[T]) =
    new Monoid[T] {
      override def empty: T = gen.from(rMonoid.value.empty)
      override def combine(x: T, y: T): T = gen.from(rMonoid.value.combine(gen.to(x), gen.to(y)))
    }

}
