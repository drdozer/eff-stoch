package stoch

import org.atnos.eff.Eff
import org.atnos.eff.all.send
import shapeless.{HList, HNil, ProductTypeClass, ProductTypeClassCompanion}
import stoch.StochasticEffect._

import scala.annotation.implicitNotFound

@implicitNotFound("Could not sample an instance of ${T}")
trait Sample[T] {
  def run[R :_stochastic]: Eff[R, T]
}

object Sample extends ProductTypeClassCompanion[Sample] {

  def apply[T](implicit s: Sample[T]): Sample[T] = s

  def const[T](t: T): Sample[T] = new Sample[T] {
    override def run[R:_stochastic]: Eff[R, T] = Eff.pure(t)
  }

  implicit object sampleInt extends Sample[Int] {
    override def run[R:_stochastic]: Eff[R, Int] =
      nextInt
  }

  implicit object sampleBoolean extends Sample[Boolean] {
    override def run[R:_stochastic]: Eff[R, Boolean] =
      nextBoolean
  }

  override val typeClass: ProductTypeClass[Sample] = new ProductTypeClass[Sample] {
    override def product[H, T <: HList](ch: Sample[H], ct: Sample[T]): Sample[shapeless.::[H, T]] = new Sample[shapeless.::[H, T]] {
      override def run[R:_stochastic]: Eff[R, shapeless.::[H, T]] =
        for {
          h <- ch.run
          t <- ct.run
        } yield h :: t
    }

    override def project[F, G](instance: => Sample[G], to: (F) => G, from: (G) => F): Sample[F] = new Sample[F] {
      override def run[R:_stochastic]: Eff[R, F] = instance.run.map(from)
    }

    override def emptyProduct: Sample[HNil] = const(HNil)
  }
}

@implicitNotFound("Cannot sample an instance using ${S}")
trait Given[S] {
  type Out
  def run[R:_stochastic](s: S): Eff[R, Out]
}

object Given {
  type Aux[S, T] = Given[S] { type Out = T }

  def apply[S, T](implicit g: Given.Aux[S, T]): Given.Aux[S, T] = g

  def apply[S](s: S)(implicit g: Given[S]): Sample[g.Out] = new Sample[g.Out] {
    override def run[R:_stochastic]: Eff[R, g.Out] = g.run(s)
  }


  implicit def memberOf[T]: Given.Aux[IndexedSeq[T], T] = new Given[IndexedSeq[T]] {
    type Out = T
    override def run[R:_stochastic](s: IndexedSeq[T]): Eff[R, T] =
      Given(Interval.Int(s.length)).run[R] map (s(_))
  }

  implicit val intGivenInterval: Given.Aux[Interval[Int], Int] = new Given[Interval[Int]] {
    type Out = Int
    override def run[R:_stochastic](r: Interval[Int]): Eff[R, Int] = {
      val width = r.max - r.min
      nextInt(width) map (_ + r.min)
    }
  }

  implicit val doubleGivenInterval: Given.Aux[Interval[Double], Double] = new Given[Interval[Double]] {
    type Out = Double
    override def run[R:_stochastic](s: Interval[Double]): Eff[R, Double] = {
      val width = s.max - s.min
      nextDouble map (_ * width + s.min)
    }
  }

  implicit val doubleGivenGauss: Given.Aux[Gauss, Double] = new Given[Gauss] {
    type Out = Double
    override def run[R:_stochastic](g: Gauss): Eff[R, Double] =
      nextGaussian map (_ * g.stdev + g.mean)
  }

}

case class Interval[T](min: T, max: T)

object Interval {
  object Int {
    def apply(max: Int) = Interval(0, max)
    def apply(min: Int, max: Int) = Interval(min, max)
  }

  object Double {
    def apply(max: Double) = Interval(0.0, max)
    def apply(min: Double, max: Double) = Interval(min, max)
  }
}

case class Gauss(mean: Double, stdev: Double)