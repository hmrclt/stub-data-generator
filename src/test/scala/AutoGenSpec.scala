package uk.gov.hmrc.smartstub

import org.scalatest.prop.Checkers
import org.scalatest._
import org.scalacheck._

class AutoGenSpec extends FlatSpec with Checkers with Matchers {

  import AutoGen._

  "The AutoGen" should "derive a Gen for a HList" in {
    import shapeless.labelled._    
    import shapeless._
    val x = 'forename
    val y = 'surname
    "val gen = AutoGen[FieldType[x.type,String] :: FieldType[y.type,String] :: HNil]" should compile
  }

  it should "derive a Gen for simple values" in {
    "val gen = AutoGen[String]" should compile
  }

  it should "derive a Gen[A] for a case class" in {
    case class Blah(forename: String, surname: String)
    "val gen: Gen[Blah] = AutoGen[Blah]" should compile
  }

  it should "derive a Gen[A] for an n-tuple" in {
    "val gen = AutoGen[(String,String,String)]" should compile
  }

  it should "derive a Gen[A] for Options, Lists and Seq's" in {
    case class Blah(forenames: List[String], surnames: Seq[String], count: Option[Int])
    "val gen: Gen[Blah] = AutoGen[Blah]" should compile
  }
}
