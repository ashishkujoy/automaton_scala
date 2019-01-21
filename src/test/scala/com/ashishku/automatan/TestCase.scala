package com.ashishku.automatan

import play.api.libs.json._

sealed trait Tuple

case class TestCase(
                name: String,
                `type`: String,
                `pass-cases`: List[String],
                `fail-cases`: List[String],
                tuple: Tuple
              )

case class NfaTuple(
                     states: List[String],
                     alphabets: List[String],
                     `start-state`: String,
                     `final-states`: List[String],
                     delta: Map[String, Map[String, List[String]]]
                   ) extends Tuple

case class DfaTuple(
                     states: List[String],
                     alphabets: List[String],
                     `start-state`: String,
                     `final-states`: List[String],
                     delta: Map[String, Map[String, String]]
                   ) extends Tuple

object NfaTuple {
  implicit val nfaTupleReads: Reads[NfaTuple] = Json.reads[NfaTuple]
}

object DfaTuple {
  implicit val dfaTupleReads: Reads[DfaTuple] = Json.reads[DfaTuple]
}

object TestCase {
  implicit val testDataReads: Reads[TestCase] = (json: JsValue) => {
    val name = (json \ "name").as[String]
    val `type` = (json \ "type").as[String]
    val `pass-cases` = (json \ "pass-cases").as[List[String]]
    val `fail-cases` = (json \ "fail-cases").as[List[String]]
    val tuple: Tuple = (json \ "type").get match {
      case JsString("dfa") => (json \ "tuple").as[DfaTuple]
      case _ => (json \ "tuple").as[NfaTuple]
    }
    JsSuccess(TestCase(name, `type`, `pass-cases`, `fail-cases`, tuple))
  }
}

case class TestData(testData: List[TestCase])

object TestData {
  implicit val testDataJson: Reads[TestData] = Json.reads[TestData]
}
