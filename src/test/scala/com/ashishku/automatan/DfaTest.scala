package com.ashishku.automatan

import org.scalatest.{FunSuite, Matchers}

class DfaTest extends FunSuite with Matchers {
  private val states = Seq("q1", "q2")
  private val alphabets = Seq("0", "1")
  private val startState = "q1"
  private val finalStates = Seq("q2")
  private val delta = Map(
    "q1" -> Map("0" -> "q2", "1" -> "q1"),
    "q2" -> Map("0" -> "q1", "1" -> "q2")
  )

  private val dfa = Dfa(states,startState,finalStates,delta)

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
