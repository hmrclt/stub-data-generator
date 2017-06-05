package hmrc.smartstub

import org.scalacheck._
import Gen._

trait Addresses extends Any {
  import Addresses._

  /** A generator that produces a semi-plausible (but fake) domestic
    * address. Postcode prefixes and their corresponding towns are
    * real but the street names, second part of the postcode and house
    * numbers are random.
    */
  def ukAddress : Gen[List[String]] = for {
    addressLetter <- frequency(
      (50,None),(5,Some("A")),(5,Some("B")),(3,Some("C"))
    )
    addressNumber <- choose(1,150)
    (codePrefix,town) <- postcodeRegions
    street <- streetNames
    postcode <- postcodeSuffix
  } yield List(
    addressNumber.toString() ++ addressLetter.getOrElse(""),
    street,
    town,
    codePrefix ++ postcode
  )

  def postcode: Gen[String] = for {
    pre <- postcodeRegions.map(_._1)
    post <- postcodeSuffix
  } yield {
    pre ++ post
  }

  def ukPhoneNumber: Gen[String] = for {
    prefix <- listOfN(4, numChar).map{_.mkString}
    suffix <- listOfN(6, numChar).map{_.mkString}
  } yield {
    s"0$prefix $suffix"
  }
}

object Addresses extends Loader {
  private[smartstub] lazy val streetNames = loadFile("streets.txt")
  private[smartstub] lazy val postcodeRegions = loadFile("postcodes.txt").map{
    x => (x.split(":").head, x.dropWhile(_ != ':').tail)
  }

  val postcodeSuffix: Gen[String] =
    listOfN(2,alphaUpperChar).map(_.mkString).flatMap{
      n => listOfN(3,choose(0,9)).map(_.mkString).map{
        x => s"${x.init} ${x.last}${n}"
      }
    }

}
