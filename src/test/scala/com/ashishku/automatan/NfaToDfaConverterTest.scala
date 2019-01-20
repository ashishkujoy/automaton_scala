package com.ashishku.automatan

import com.ashishku.automatan.LiveState.create
import org.scalatest.{FunSuite, Matchers}

class NfaToDfaConverterTest extends FunSuite with Matchers {

  private val stateQ1: LiveState = create("q1")
  private val stateQ2: LiveState = create("q2")
  private val stateQ3: LiveState = create("q3")
  private val stateQ1Q2: LiveState = create(List("q1", "q2"))
  private val stateQ2Q3: LiveState = create(List("q2", "q3"))
  private val stateQ1Q3: LiveState = create(List("q1", "q3"))
  private val nfaStates = List("q1", "q2", "q3")
  private val stateQ1Q2Q3: LiveState = create(nfaStates)

  private val dfaStates = List(
    DeadState,
    stateQ1, stateQ2, stateQ3,
    stateQ1Q2, stateQ2Q3, stateQ1Q3, stateQ1Q2Q3
  )

  test("should give states for a dfa from states of nfa") {
    NfaToDfaConverter.toDfaStates(nfaStates) should contain theSameElementsAs dfaStates
  }

  test("should give final states for a dfa based on final states of nfa") {
    val expectedStates = Seq(stateQ1, stateQ1Q2, stateQ1Q3, stateQ1Q2Q3)
    NfaToDfaConverter.dfaFinalStates(dfaStates, List("q1")) should contain theSameElementsAs expectedStates
  }

  test("should give delta for dfa for given dfa states and nfa") {
    val nfaDelta = Map(
      "q1" -> Map("e" -> Seq("q3"), "b" -> Seq("q2")),
      "q2" -> Map("a" -> Seq("q2", "q3"), "b" -> Seq("q3")),
      "q3" -> Map("a" -> Seq("q3"))
    )
    val alphabets = Seq("a", "b")

    val dfaDelta = Map(
      stateQ1 -> Map("a" -> DeadState, "b" -> stateQ2),
      stateQ2 -> Map("a" -> stateQ2Q3, "b" -> stateQ3),
      stateQ3 -> Map("a" -> stateQ1Q3, "b" -> DeadState),
      stateQ1Q2 -> Map("a" -> stateQ2Q3, "b" -> stateQ2Q3),
      stateQ1Q3 -> Map("a" -> stateQ1Q3, "b" -> stateQ2),
      stateQ2Q3 -> Map("a" -> stateQ1Q2Q3, "b" -> stateQ3),
      stateQ1Q2Q3 -> Map("a" -> stateQ1Q2Q3, "b" -> stateQ2Q3),
    )

    val actualDfaDelta = NfaToDfaConverter.dfaDelta(Nfa(nfaStates, alphabets, "q1", Seq("q3"), nfaDelta), dfaStates)

    actualDfaDelta(stateQ1) shouldBe dfaDelta(stateQ1)
  }

  test("should convert nfa to dfa") {
    val nfaDelta = Map(
      "q1" -> Map("e" -> Seq("q3"), "b" -> Seq("q2")),
      "q2" -> Map("a" -> Seq("q2", "q3"), "b" -> Seq("q3")),
      "q3" -> Map("a" -> Seq("q3"))
    )
    val alphabets = Seq("a", "b")
    val nfa = Nfa(nfaStates, alphabets, "q1", Seq("q3"), nfaDelta)
    val dfa: Dfa = NfaToDfaConverter.converter(nfa)
    dfa.doesAccept("b")  shouldBe false
    dfa.doesAccept("baa")  shouldBe true
    dfa.doesAccept("a")  shouldBe true
  }

}
