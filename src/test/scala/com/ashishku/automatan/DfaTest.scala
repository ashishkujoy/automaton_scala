package com.ashishku.automatan

import com.ashishku.automatan.LiveState.create
import org.scalatest.{FunSuite, Matchers}

class DfaTest extends FunSuite with Matchers {
  private val stateQ1: LiveState = create("q1")
  private val stateQ2: LiveState = create("q2")
  private val states = List(stateQ1, stateQ2)
  private val alphabets = List("0", "1")
  private val startState = stateQ1
  private val finalStates = List(stateQ2)
  private val delta: Map[State, Map[String, State]] = Map(
    stateQ1 -> Map("0" -> stateQ2, "1" -> stateQ1),
    stateQ2 -> Map("0" -> stateQ1, "1" -> stateQ2)
  )

  private val dfa = Dfa(states, startState, finalStates, delta)

  test("should reject empty string") {
    dfa.doesAccept("") shouldBe false
  }

  test("should accept odd number of zeros") {
    dfa.doesAccept("000") shouldBe true
  }

  test("should reject even number of zeros") {
    dfa.doesAccept("0000") shouldBe false
  }

}
