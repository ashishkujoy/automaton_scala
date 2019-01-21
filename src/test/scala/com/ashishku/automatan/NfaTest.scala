package com.ashishku.automatan

import org.scalatest.{FunSpec, Matchers}

class NfaTest extends FunSpec with Matchers {

  describe("Nfa") {

    val states = List("q1", "q3", "q7", "q2", "q5", "q6", "q4", "q8")
    val alphabets = List("1", "0")
    val delta = Map(
      "q1" -> Map("e" -> List("q2", "q5")),
      "q2" -> Map("0" -> List("q3")),
      "q3" -> Map("1" -> List("q4"), "e" -> List("q6")),
      "q4" -> Map("0" -> List("q3")),
      "q5" -> Map("1" -> List("q6"), "0" -> List("q8")),
      "q6" -> Map("0" -> List("q7"), "e" -> List("q3", "q4")),
      "q7" -> Map("1" -> List("q6"))
    )
    val startState = "q1"
    val finalState = List("q3", "q6")
    val nfa = Nfa(states, alphabets, startState, finalState, delta)

    describe("coexistingGroupOf") {

      it("should give all states connected by epsilon") {
        nfa.coexistingGroupOf("q1") shouldBe List("q2", "q5", "q1")
      }

      it("should give all states in case state have close loop of epsilon") {
        nfa.coexistingGroupOf("q6") shouldBe List("q3", "q4", "q6")
      }
    }

    describe("next") {

      it("should give next state from a given state") {
        nfa.nextStates("q7", "1") should contain only("q6", "q3", "q4")
      }

      it("should give empty array when their is no output from state for given input") {
        nfa.nextStates("q7", "0") shouldBe List.empty
      }

      it("should give empty array when their is no delta for given state") {
        nfa.nextStates("q8", "0") shouldBe List.empty
        nfa.nextStates("q8", "1") shouldBe List.empty
      }

    }

    describe("doesAccept") {
      describe("validInput") {

        it("should accept 0") {
          nfa.doesAccept("0") shouldBe true
        }

        it("should accept 010") {
          nfa.doesAccept("010") shouldBe true
        }

      }

      describe("invalidInput")  {

        it("should reject empty string") {
          nfa.doesAccept("") shouldBe false
        }

        it("should reject 01") {
          nfa.doesAccept("01") shouldBe false
        }
      }

    }
  }
}
