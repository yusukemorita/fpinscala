package fpinscala.state

import org.scalatest._

class Exercise6_11Spec extends FlatSpec {

  val lockedMachine: Machine = Machine(locked = true, candies = 10, coins = 10)
  val unlockedMachine: Machine = Machine(locked = false, candies = 10, coins = 10)

  // Inserting a coin into a locked machine will cause it to
  // unlock if there’s any candy left.

  "inserting a coin when locked" should "unlock machine" in {
    val inputCoin = State.simulateMachine(List(Coin))
    val ((_, _), nextMachine) = inputCoin.run(lockedMachine)
    assert(nextMachine.locked === false)
  }

  // Turning the knob on an unlocked machine will cause it to dispense candy and become locked.

  "turning the knob on an unlocked machine with candies left" should "dispense candy" in {
    val turnKnob = State.simulateMachine(List(Turn))
    val ((nextCandies, _), nextMachine) = turnKnob.run(unlockedMachine)

    assert(nextMachine.locked === true)
    assert(nextCandies === unlockedMachine.candies - 1)
  }

  // Inserting a coin and turning the knob should dispense candy and relock machine

  "inserting a coin and turning the knob on a locked machine with candies left" should "dispense candy" in {
    val action = State.simulateMachine(List(Coin, Turn))
    val ((nextCandies, _), nextMachine) = action.run(lockedMachine)

    assert(nextMachine.locked === true)
    assert(nextCandies === lockedMachine.candies - 1)
  }

  // Turning the knob on a locked machine or inserting a coin into an unlocked machine does nothing.

  "turning the knob when locked" should "do nothing" in {
    val inputCoin = State.simulateMachine(List(Turn))
    val ((_, _), nextMachine) = inputCoin.run(lockedMachine)
    assert(nextMachine === lockedMachine)
  }

  "inserting a coin when unlocked" should "do nothing" in {
    val inputCoin = State.simulateMachine(List(Coin))
    val ((_, _), nextMachine) = inputCoin.run(unlockedMachine)
    assert(nextMachine === unlockedMachine)
  }

  // A machine that’s out of candy ignores all inputs.

  "Turning the knob when out of candy" should "do nothing" in {
    val lockedMachineWithNoCandy = lockedMachine.copy(candies = 0)
    val unlockedMachineWithNoCandy = unlockedMachine.copy(candies = 0)
    val turn = State.simulateMachine(List(Turn))
    val (_, nextLockedMachine) = turn.run(lockedMachineWithNoCandy)
    val (_, nextUnlockedMachine) = turn.run(unlockedMachineWithNoCandy)

    assert(nextLockedMachine == lockedMachineWithNoCandy)
    assert(nextUnlockedMachine == unlockedMachineWithNoCandy)
  }

  "Inserting a coin when out of candy" should "do nothing" in {
    val lockedMachineWithNoCandy = lockedMachine.copy(candies = 0)
    val unlockedMachineWithNoCandy = unlockedMachine.copy(candies = 0)
    val inputCoin = State.simulateMachine(List(Coin))

    val (_, nextLockedMachine) = inputCoin.run(lockedMachineWithNoCandy)
    val (_, nextUnlockedMachine) = inputCoin.run(unlockedMachineWithNoCandy)

    assert(nextLockedMachine == lockedMachineWithNoCandy)
    assert(nextUnlockedMachine == unlockedMachineWithNoCandy)
  }

}
