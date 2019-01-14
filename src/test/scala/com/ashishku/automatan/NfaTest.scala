package com.ashishku.automatan

import org.scalatest.{FunSpec, Matchers}

class NfaTest extends FunSpec with Matchers {
  describe("Nfa") {
    describe("coexistingGroupOf") {
      val states  = Seq("q1", "q3", "q7", "q2", "q5", "q6", "q4")
      val alphabets = Seq("1", "0")
      val delta =  Map(
        "q1" -> Map( "e" -> Seq("q2", "q5") ),
        "q2" -> Map( "0" -> Seq("q3") ),
        "q3" -> Map( "1" -> Seq("q4"), "e" -> Seq("q6") ),
        "q4" -> Map( "0" -> Seq("q3") ),
        "q5" -> Map( "1" -> Seq("q6") ),
        "q6" -> Map( "0" -> Seq("q7"),"e"  -> Seq("q3","q4")),
        "q7" -> Map( "1" -> Seq("q6"))
        )
      val startState =  "q1"
      val finalState =  Seq("q3", "q6")
      val nfa = Nfa(states, alphabets,startState,finalState,delta)

      it("should give all states connected by epsilon") {
        nfa.coexistingGroupOf("q1") shouldBe Seq("q2","q5","q1")
      }

      it("should give all states in case state have close loop of epsilon") {
        nfa.coexistingGroupOf("q6") shouldBe Seq("q3","q4","q6")
      }
    }
  }
}
