package com.ashishku.automatan

case class Nfa(
                states: Seq[String],
                alphabets: Seq[String],
                currentState: String,
                finalStates: Seq[String],
                delta: Map[String, Map[String, Seq[String]]]
              ) {

  def coexistingGroupOf(state: String, excluding: Seq[String] = Seq.empty): Seq[String] = {
    val deltaStates = delta.getOrElse(state, Map.empty).getOrElse("e", Seq.empty).filterNot(state => excluding.contains(state))
    deltaStates.flatMap(deltaState => coexistingGroupOf(deltaState, excluding :+ state)) :+ state
  }

  def nextStates(state: String, input: String): Seq[String] = {
    coexistingGroupOf(state)
      .flatMap(currentState => delta.getOrElse(currentState, Map.empty).getOrElse(input, Seq.empty))
      .flatMap(state => coexistingGroupOf(state))
  }
}
