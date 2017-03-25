package stoch

import cats.instances.all._
import cats.syntax.all._
import org.atnos.eff._
import org.atnos.eff.all._
import stoch.StochasticEffect._

import scala.util.Random

/**
  * Created by nmrp3 on 25/03/17.
  */
object SampleDice {

  // A roll of a dice
  case class DiceRole(number: Int)

  // The sampler for dice roles
  implicit val sampleDiceRole = new Sample[DiceRole] {
    override def run[R: _stochastic]: Eff[R, DiceRole] =
      Given(Interval.Int(1, 6)).run map DiceRole.apply
  }

  case class TwoRolls(role1: DiceRole, role2: DiceRole) {
    def pretty: String = s"(${role1.number}, ${role2.number}) = ${role1.number + role2.number}"
  }

  def main(args: Array[String]): Unit = {
    def roll5Times[R : _stochastic] =
      List.fill(5)(Sample[TwoRolls].run[R]).sequence

    type Stack = Fx1[Stochastic]

    for {
      r <- run(runStochastic(new Random, roll5Times[Stack]))
    } println(s"Dice roll: ${r.pretty}")
  }

}
