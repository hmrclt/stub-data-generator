package hmrc.smartstub

import org.scalacheck._
import Gen._

sealed trait Gender
case object Male extends Gender
case object Female extends Gender

trait Names extends Any {
  def surname = Names.surname
  def _forenames = Names._forenames

  def gender: Gen[Gender] = oneOf(Male, Female)
  def forename(): Gen[String] = for {
    g <- gender
    v <- _forenames(g)
  } yield (v)
  def forename(g: Gender): Gen[String] = _forenames(g)

}

object Names extends Loader {
  lazy val surname: Gen[String] = loadWeightedFile("surnames.txt")
  println("surname is " + surname)
  lazy val _forenames: Map[Gender, Gen[String]] = Map(
    ( Male -> loadFile("forenames-male.txt")),
    ( Female -> loadFile("forenames-female.txt"))
  )
}
