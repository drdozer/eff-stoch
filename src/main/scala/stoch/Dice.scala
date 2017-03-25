package stoch

import cats.syntax.all._
import cats.instances.all._
import org.atnos.eff._
import org.atnos.eff.all._
import StochasticEffect._

import scala.util.Random

/**
  * Created by nmrp3 on 25/03/17.
  */
object Dice {

  case class TwoRolls(dice1: Int, dice2: Int) {
    def pretty: String = s"($dice1, $dice2) = ${dice1 + dice2}"
  }

  def main(args: Array[String]): Unit = {
    def diceRoll[R :_stochastic] = nextInt(6) map (_ + 1)

    def rollTwoDice[R :_stochastic] = for {
      d1 <- diceRoll
      d2 <- diceRoll
    } yield TwoRolls(d1, d2)

    def roll5Times[R :_stochastic] =
      List.fill(5)(rollTwoDice).sequence

    type Stack = Fx1[Stochastic]

    for {
      r <- run(runStochastic(new Random, roll5Times[Stack]))
    } println(s"Dice roll: ${r.pretty}")
  }

}
