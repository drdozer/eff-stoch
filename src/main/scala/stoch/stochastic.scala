package stoch

import scala.util.Random
import org.atnos.eff._
import org.atnos.eff.all._

sealed trait Stochastic[A]
case class Get[A](run: Random => A) extends Stochastic[A]

trait StochasticTypes {
  type _Stochastic[R] = Stochastic /= R
  type _stochastic[R] = Stochastic |= R
}

trait StochasticCreation extends StochasticTypes {

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

}

object StochasticCreation extends StochasticCreation

trait StochasticInterpretation extends StochasticTypes {

  def runStochastic[R, U, A](r: Random, effect: Eff[R, A])(implicit _stochastic: Member.Aux[Stochastic, R, U]): Eff[U, A] =
    Interpret.translate(effect)(new Translate[Stochastic, U] {
      def apply[X](s: Stochastic[X]): Eff[U, X] = s match {
        case Get(run) => Eff.pure(run(r))
      }
    }
  )

}

object StochasticInterpretation extends StochasticInterpretation

trait StochasticEffect extends StochasticCreation with StochasticInterpretation

object StochasticEffect extends StochasticEffect

object stochastic extends stochastic

trait stochastic {

  implicit class StochasticEffectOps[R, A](e: Eff[R, A]) {

    def runStochastic[U](r: Random)(implicit member: Member.Aux[Stochastic, R, U]): Eff[U, A] =
      StochasticInterpretation.runStochastic(r, e)

  }

}