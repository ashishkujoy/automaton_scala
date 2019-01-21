package com.ashishku.automatan

case class Dfa(
                states: List[State],
                currentState: State,
                finalStates: List[State],
                delta: Map[State, Map[String, State]]
              ) {
  def doesAccept(language: String): Boolean = {
    val newState = language.split("").filterNot(_.isEmpty).foldLeft(currentState) { (state, input) =>
      delta(state)(input)
    }
    finalStates.contains(newState)
  }
}
