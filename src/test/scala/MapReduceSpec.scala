import org.scalatest._
import cats._
import cats.syntax.cartesian._
import cats.syntax.monoid._
import cats.syntax.applicative._
import cats.syntax.functor._
import cats.kernel.instances.all._
import MonoidInstances._
import Fold._
import cats.syntax.CartesianBuilder

case class SomeThing(i: Int, d: Double)

class MapReduceSpec extends WordSpec with Matchers {

  "derived monoid instances work" when {
    "case class is combineable" in {
      val a = SomeThing(1, 3.2)
      val b = SomeThing(2, 4.5)
      val c = a |+| b
      c shouldEqual SomeThing(3, 7.7)
    }
  }

  "fold over stuff" in {
    val f = Fold((x: SomeThing) => x, (x: SomeThing) => x)(implicitly[Monoid[SomeThing]])
    val result = f.fold(List(SomeThing(1, 3.2), SomeThing(2, 4.5)))
    result shouldEqual SomeThing(3, 7.7)
  }

  "mapping" when {

    "provide tally and summarize with appropriate monoid" in {
      val f = Fold((x: SomeThing) => x.i, ((x: Int) => x))
      f.fold(List(SomeThing(1, 3.2), SomeThing(2, 4.5))) shouldEqual 3
    }

    "map over fold" in {
      val f = Fold((x: SomeThing) => x, (x: SomeThing) => x) 
      val g = f map (x => x.i)
      g.fold(List(SomeThing(1, 3.2), SomeThing(2, 4.5))) shouldEqual 3
    }

    "replacing monoid" in {
      val prodMonoid = new Monoid[Int] {
        val empty =1
        def combine(x: Int, y: Int) = x * y
      }

      val length = Fold((s:String) => s.length, (l:Int) => s"the product of string length is ${l.toString}")(prodMonoid)
      length.run("a","b") shouldEqual "the product of string length is 1"
    }

    "applicative average" in {
      val sum = Fold((x: Int) => x,(x: Int) => x)

      def length[A] = Fold((_:A) => 1, (x: Int) => x)

      val average: Fold[Int,Double] =   sum |@| length map ((x,y) => x / y)

      average.run(1,2,3) shouldEqual 2.0
    }

  }
  
}
