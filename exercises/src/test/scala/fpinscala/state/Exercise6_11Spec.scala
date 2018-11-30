package fpinscala.state

import fpinscala.state.RNG.Rand
import org.scalatest._

class Exercise6_11Spec extends FlatSpec {

  // Inserting a coin into a locked machine will cause it to
  // unlock if there’s any candy left.

  "inserting a coin when locked and candies left" should "unlock machine" in {
    val machine: Machine = Machine(locked = true, candies = 1, coins = 0)
    val inputCoin = State.simulateMachine(List(Coin))
    val ((_, _), nextMachine) = inputCoin.run(machine)
    assert(nextMachine.locked === false)
  }

  "inserting a coin when locked and no candies left" should "not unlock machine" in {
    val machine: Machine = Machine(locked = true, candies = 0, coins = 0)
    val inputCoin = State.simulateMachine(List(Coin))
    val ((_, _), nextMachine) = inputCoin.run(machine)
    assert(nextMachine.locked === true)
  }
}
