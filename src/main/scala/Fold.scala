import cats._
import cats.syntax.monoid._
import cats.instances.all._
import shapeless._
import shapeless.syntax._
import MonoidInstances._

sealed trait Fold[A, B] {
  type C
  def tally: A => C
  def summarize: C => B
  implicit val m: Monoid[C]
  def fold(xs: List[A]): B = summarize(xs.foldLeft(m.empty)((x, y) => x |+| tally(y)))
  def run(xs: A*): B = fold(xs.toList)
}

object Fold {
  def apply[A, B, M](t: A => M, s: M => B)(implicit mon: Monoid[M]): Fold[A, B] = new Fold[A, B] {
    type C = M
    implicit val m = mon
    override def tally: A => C = t
    override def summarize: C => B = s
  }

  implicit def applicativeFold[From] = new Applicative[({ type F[X] = Fold[From, X] })#F] {
    override def pure[A](x: A): Fold[From, A] = new Fold[From, A] {
      override type C = HNil

      override def tally: (From) => HNil = _ => HNil
      override def summarize: HNil => A = _ => x
      override implicit val m: Monoid[HNil] = MonoidInstances.hNilMonoid
    }

    override def ap[A, B](ff: Fold[From, A => B])(fa: Fold[From, A]): Fold[From, B] = new Fold[From, B] {
      private implicit val ffm: Monoid[ff.C] = ff.m
      private implicit val fam: Monoid[fa.C] = fa.m
      override type C = ff.C :: fa.C :: HNil
      override implicit val m: Monoid[C] = MonoidInstances.hConsMonoid[ff.C, fa.C :: HNil]
      override def tally: (From) => C = x => ff.tally(x) :: fa.tally(x) :: HNil
      override def summarize: C => B = x => {
        val f = ff.summarize(x.head)
        val a = fa.summarize(x.tail.head)
        f(a)
      }
    }

  }

}
