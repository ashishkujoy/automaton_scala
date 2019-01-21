package com.ashishku.automatan

case class Nfa(
                states: List[String],
                alphabets: List[String],
                currentState: String,
                finalStates: List[String],
                delta: Map[String, Map[String, List[String]]]
              ) {
  def doesAccept(input: String): Boolean = {
    val sanitisedInputs = input.trim.replaceAllLiterally("*", "").split("").filter(input => input != "")
    val newStates = sanitisedInputs.foldLeft(coexistingGroupOf(currentState, List(currentState))) { (states, input) =>
      states.flatMap(state => coexistingGroupOf(state).flatMap(s => nextStates(s, input)))
    }
    newStates.exists(state => finalStates.contains(state))
  }


  def coexistingGroupOf(state: String, excluding: List[String] = List.empty): List[String] = {
    val deltaStates = delta.getOrElse(state, Map.empty).getOrElse("e", List.empty).filterNot(state => excluding.contains(state))
    deltaStates.flatMap(deltaState => coexistingGroupOf(deltaState, excluding :+ state)) :+ state
  }

  def nextStates(currentState: String, input: String): List[String] = {
    delta.getOrElse(currentState, Map.empty).getOrElse(input, List.empty)
      .flatMap(state => coexistingGroupOf(state))
  }
}
