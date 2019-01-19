package com.ashishku.automatan

trait State

case object DeadState extends State

case class LiveState(stateElements: Set[String]) extends State

object LiveState {
  def create(element: String): LiveState = LiveState(Set(element))

  def create(element: List[String]): LiveState = LiveState(Set(element: _*))
}