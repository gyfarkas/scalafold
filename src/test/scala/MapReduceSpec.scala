import org.scalatest._
import cats._
import cats.syntax.all._
import cats.kernel.instances.all._
import MonoidInstances._
import Fold._

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
    val f = Fold((x: SomeThing) => x, (x: SomeThing) => x)
    val result = f.fold(List(SomeThing(1, 3.2), SomeThing(2, 4.5)))
    result shouldEqual SomeThing(3, 7.7)
  }

  "mapping" when {

    "provide tally and summarize with appropriate monoid" in {
      val f = Fold((x: SomeThing) => x.i, (x: Int) => x)
      f.fold(List(SomeThing(1, 3.2), SomeThing(2, 4.5))) shouldEqual 3
    }

    "map over fold" in {
      val f = Fold((x: SomeThing) => x, (x: SomeThing) => x)
      val g = f map (x => x.i)
      g.fold(List(SomeThing(1, 3.2), SomeThing(2, 4.5))) shouldEqual 3
    }

    "replacing monoid" in {
      val prodMonoid = new Monoid[Int] {
        val empty = 1
        def combine(x: Int, y: Int) = x * y
      }

      val length = Fold((s: String) => s.length, (l: Int) => s"the product of string length is ${l.toString}")(prodMonoid)
      length.run("a", "b") shouldEqual "the product of string length is 1"
    }

    "applicative average" in {
      val sum = Fold((x: Int) => x.toDouble, (x: Double) => x)
      val sumSq = Fold((x: Int) => (x * x).toDouble, (x: Double) => x)
      def length[A] = Fold((_: A) => 1.0, (x: Double) => x)

      val average: Fold[Int, Double] = (sum |@| length) map ((x, y) => x / y)
      val variance: Fold[Int, Double] =
        (sumSq |@| sum |@| length)
          .map((sq, s, l) => sq / l - Math.pow(s / l, 2))

      val sd = variance.map(x => Math.sqrt(x))

      val all: Fold[Int, (Double, Double, Double)] =
        (average |@| variance |@| sd)
          .map((a, v, s) => (a, v, s))

      val (averageResult, varianceResult, stdDevResult) = all.run(1, 2, 3)

      averageResult shouldEqual 2.0
      stdDevResult shouldEqual 0.8164965809277263
      varianceResult shouldEqual 0.666666666666667
    }

    "word count" in {
      val mapping = Fold(
        (x: String) => List((x, 1)),
        (x: List[(String, Int)]) => x
      ).map(_.groupBy(_._1))

      val reducing = Fold(
        (m: Map[String, List[(String, Int)]]) => m.map { case (k, v) => (k, v.length) }.toList,
        (x: List[(String, Int)]) => x
      )

      val result = reducing.run(mapping.run("abc", "abc", "def"))
      result should contain theSameElementsAs List(("abc", 2), ("def", 1))
    }

  }

}
