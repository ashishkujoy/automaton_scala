package com.ashishku.automatan


object NfaToDfaConverter {

  def dfaDelta(nfa: Nfa, dfaStates: List[State]): Map[State, Map[String, State]] = {
    dfaStates.foldLeft(Map.empty[State, Map[String, State]]) { (nfaDelta, dfaState) =>
      val stringToState = nfa.alphabets.foldLeft(Map.empty[String, State]) { (strToState, alphabet) =>
        strToState.updated(alphabet, nextStateFor(dfaState, alphabet, nfa))
      }
      nfaDelta.updated(dfaState, stringToState)
    }
  }


  def toDfaStates(nfaStates: List[String]): List[State] = {
    val liveStates = List.range(1, nfaStates.size + 1, 1)
      .flatMap(combSize => nfaStates.combinations(combSize))
      .map(stateElements => LiveState.create(stateElements))
    liveStates :+ DeadState
  }

  def dfaFinalStates(dfaStates: List[State], nfaFinalStates: List[String]): List[State] = {
    nfaFinalStates.flatMap(nfaFinalState => dfaStates.filter {
      case liveState: LiveState => liveState.stateElements.contains(nfaFinalState)
      case _ => false
    })
  }

  def converter(nfa: Nfa): Dfa = {
    val dfaStates = toDfaStates(nfa.states.toList)
    Dfa((dfaStates), dfaCurrentState(nfa.currentState, nfa), dfaFinalStates(dfaStates, nfa.finalStates.toList), dfaDelta(nfa, dfaStates))
  }

  private def nextStateFor(state: State, alphabet: String, nfa: Nfa): State = {
    state match {
      case liveState: LiveState =>
        val stateElements = liveState.stateElements.flatMap(stateEle => nfa.nextStates(stateEle, alphabet))
        if (stateElements.isEmpty) DeadState else LiveState(stateElements)
      case deadState => deadState
    }
  }

  private def dfaCurrentState(nfaCurrentState: String, nfa: Nfa): LiveState = {
    LiveState.create(nfa.coexistingGroupOf(nfaCurrentState).toList)
  }
}
