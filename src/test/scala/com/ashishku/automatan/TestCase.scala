package com.ashishku.automatan

import play.api.libs.json._

sealed trait Tuple

case class TestCase(
                name: String,
                `type`: String,
                `pass-cases`: Seq[String],
                `fail-cases`: Seq[String],
                tuple: Tuple
              )

case class NfaTuple(
                     states: Seq[String],
                     alphabets: Seq[String],
                     `start-state`: String,
                     `final-states`: Seq[String],
                     delta: Map[String, Map[String, Seq[String]]]
                   ) extends Tuple

case class DfaTuple(
                     states: Seq[String],
                     alphabets: Seq[String],
                     `start-state`: String,
                     `final-states`: Seq[String],
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
    val `pass-cases` = (json \ "pass-cases").as[Seq[String]]
    val `fail-cases` = (json \ "fail-cases").as[Seq[String]]
    val tuple: Tuple = (json \ "type").get match {
      case JsString("dfa") => (json \ "tuple").as[DfaTuple]
      case _ => (json \ "tuple").as[NfaTuple]
    }
    JsSuccess(TestCase(name, `type`, `pass-cases`, `fail-cases`, tuple))
  }
}

case class TestData(testData: Seq[TestCase])

object TestData {
  implicit val testDataJson: Reads[TestData] = Json.reads[TestData]
}
