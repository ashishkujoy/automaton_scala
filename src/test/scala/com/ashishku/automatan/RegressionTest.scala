package com.ashishku.automatan

import org.scalatest.{FunSpec, FunSuite, Matchers, WordSpec}
import play.api.libs.json.Json

import scala.io.Source.fromResource

class RegressionTest extends FunSpec with Matchers {
  private val source: String = fromResource("testData.json").mkString
  private val testData: TestData = Json.parse(source).as[TestData]

  private val dfaTestCases: Seq[TestCase] = testData.testData.filter(_.`type` == "dfa")
  private val nfaTestCases: Seq[TestCase] = testData.testData.filter(_.`type` == "nfa")

  describe("Dfa") {

    dfaTestCases.foreach { dfaTestCase =>
      val tuple = dfaTestCase.tuple.asInstanceOf[DfaTuple]
      val dfa = Dfa(tuple.states, tuple.`start-state`, tuple.`final-states`, tuple.delta)

      describe(dfaTestCase.name) {

        dfaTestCase.`pass-cases`.foreach { validInput =>
          it(s"$validInput should be accepted") {
            dfa.doesAccept(validInput) shouldBe true
          }
        }

        dfaTestCase.`fail-cases`.foreach { inValidInput =>
          it(s"$inValidInput should be rejected") {
            dfa.doesAccept(inValidInput) shouldBe false
          }
        }

      }
    }
  }

  describe("Nfa") {

    nfaTestCases.foreach { nfaTestCase =>
      val tuple = nfaTestCase.tuple.asInstanceOf[NfaTuple]
      val nfa = Nfa(tuple.states, tuple.alphabets, tuple.`start-state`, tuple.`final-states`, tuple.delta)

      describe(nfaTestCase.name) {

        nfaTestCase.`pass-cases`.foreach { validInput =>
          it(s"$validInput should be accepted") {
            nfa.doesAccept(validInput) shouldBe true
          }
        }

        nfaTestCase.`fail-cases`.foreach { inValidInput =>
          it(s"$inValidInput should be rejected") {
            nfa.doesAccept(inValidInput) shouldBe false
          }
        }

      }
    }

  }
}
