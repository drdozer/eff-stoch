package stoch

import scala.util.Random
import org.atnos.eff._
import org.atnos.eff.all._

sealed trait Stochastic[A]
case class Get[A](run: Random => A) extends Stochastic[A]

object StochasticEffect {

  type _stochastic[R] = Stochastic |= R

  def nextInt[R :_stochastic]: Eff[R, Int] =
    send[Stochastic, R, Int](Get(r => r.nextInt))

  def nextInt[R :_stochastic](n: Int): Eff[R, Int] =
    send[Stochastic, R, Int](Get(r => r.nextInt(n)))

  def nextBoolean[R :_stochastic]: Eff[R, Boolean] =
    send[Stochastic, R, Boolean](Get(r => r.nextBoolean()))

  def nextGaussian[R :_stochastic]: Eff[R, Double] =
    send[Stochastic, R, Double](Get(r => r.nextGaussian()))

  def nextDouble[R :_stochastic]: Eff[R, Double] =
    send[Stochastic, R, Double](Get(r => r.nextDouble()))

  def runStochastic[R, U, A](r: Random, effect: Eff[R, A])(implicit _stochastic: Member.Aux[Stochastic, R, U]): Eff[U, A] =
    Interpret.translate(effect)(new Translate[Stochastic, U] {
      def apply[X](s: Stochastic[X]): Eff[U, X] = s match {
        case Get(run) => Eff.pure(run(r))
      }
    }
  )
}
