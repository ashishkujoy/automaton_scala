package com.ashishku.automatan

case class Dfa(states: Seq[String], currentState: String, finalStates: Seq[String], delta: Map[String, Map[String,String]]) {
  def doesAccept(language:String): Boolean = {
    val newState = language.split("").filterNot(_.isEmpty).foldLeft(currentState) { (state, input) =>
      delta(state)(input)
    }
    finalStates.contains(newState)
  }
}
