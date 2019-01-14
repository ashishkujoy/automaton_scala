package com.ashishku.automatan

import org.scalatest.{FunSpec, FunSuite, Matchers, WordSpec}
import play.api.libs.json.Json

import scala.io.Source.fromResource

class RegressionTest extends FunSpec with Matchers {
  private val source: String = fromResource("testData.json").mkString
  private val testData: TestData = Json.parse(source).as[TestData]

  private val dfaTestCases: Seq[TestCase] = testData.testData.filter(_.`type` == "dfa")

  describe("Dfa") {
    dfaTestCases.foreach { dfaTestCase =>
      val tuple = dfaTestCase.tuple.asInstanceOf[DfaTuple]
      val dfa = Dfa(tuple.states,tuple.`start-state`,tuple.`final-states`,tuple.delta)

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
}
