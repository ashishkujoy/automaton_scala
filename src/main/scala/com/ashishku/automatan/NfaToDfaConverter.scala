package com.ashishku.automatan

object NfaToDfaConverter {
  def dfaDelta(nfa: Nfa, dfaStates: List[State]): Map[State, Map[String, State]] = {
    dfaStates.foldLeft(Map.empty[State, Map[String, State]]) { (nfaDelta, dfaState) =>
      dfaState match {
        case liveState: LiveState => {
          val stringToState = nfa.alphabets.foldLeft(Map.empty[String, State]) { (stateFunc, alphabet) =>
            val nextStateElements = liveState.stateElements.flatMap { stateElement =>
              nfa.nextStates(stateElement, alphabet)
            }
            stateFunc.updated(alphabet, LiveState(nextStateElements))
          }
          nfaDelta.updated(dfaState, stringToState)
        }
        case _ => {
          val stringToState = nfa.alphabets.foldLeft(Map.empty[String, State]) { (stateFunc, alphabet) =>
            stateFunc.updated(alphabet, DeadState)
          }
          nfaDelta.updated(dfaState, stringToState)
        }
      }
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

  def converter(nfa: Nfa): Unit = {

  }
}
