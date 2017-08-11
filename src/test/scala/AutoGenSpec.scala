package uk.gov.hmrc.smartstub

import org.scalatest.prop.Checkers
import org.scalatest._
import org.scalacheck.Gen

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
    case class Blah(forenames: Seq[String], surnames: Set[String], other: List[String], other2: Vector[String], count: Option[Int])
    "val gen: Gen[Blah] = AutoGen[Blah]" should compile
  }

  it should "derive a Gen[A] for sealed traits" in {
    sealed trait AorB
    case class A(i: Int) extends AorB
    case class B(s: String) extends AorB
    "val gen1 = AutoGen[A]" should compile
    "val gen2 = AutoGen[B]" should compile
    "val gen3 = AutoGen[AorB]" should compile
  }

  it should "derive a Gen[A] for Eithers" in {
    "val gen = AutoGen[Either[String,Int]]" should compile
  }

  it should "derive a Gen[A] for coproducts in a case class" in {
    sealed trait Blooh
    case class Bleeh(i: Int) extends Blooh
    case object Bluh extends Blooh
    case class Blah(thing: Either[Blooh,Int])

    "val gen = AutoGen[Either[Blooh,Blah]]" should compile
  }

  it should "derive a Gen[A] for trees" in {
    sealed trait Tree
    case class Leaf(i: Int) extends Tree
    case class Branch(t: Tree) extends Tree

    "val gen = AutoGen[Tree]" should compile
    /**
      * `AutoGen[Tree]` does compile but it does result in a stack overflow exception
      * at runtime. Probably can't avoid this because of the recursive nature of the Tree.
      */
  }

  it should "derive Gen[A] for nested collections of coproducts" in {
    sealed trait T
    case object T1 extends T
    case object T2 extends T

    case class S1(s: Set[T])
    case class S2(s: Vector[T])
    case class S3(s: List[T])

    "val gen1 = AutoGen[S1]" should compile
    "val gen2 = AutoGen[S2]" should compile
    "val gen3 = AutoGen[S3]" should compile
  }

  it should "derive Gen[A] for LocalDate" in {
    import java.time.LocalDate
    case class D(date: LocalDate)
    "val gen1 = AutoGen[D]" should compile
    "val gen2 = AutoGen[LocalDate]" should compile
  }



}
