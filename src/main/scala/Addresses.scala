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
    postcode <- listOfN(2,alphaUpperChar).map(_.mkString).flatMap{
      n => listOfN(3,choose(0,9)).map(_.mkString).map{
        x => s"${codePrefix}${x.init} ${x.last}${n}"
      }
    }
  } yield List(
    addressNumber.toString() ++ addressLetter.getOrElse(""),
    street,
    town,
    postcode
  )
}

object Addresses extends Loader {
  private[smartstub] lazy val streetNames = loadFile("streets.txt")
  private[smartstub] lazy val postcodeRegions = loadFile("postcodes.txt").map{
    x => (x.split(":").head, x.dropWhile(_ != ':').tail)
  }

  private[smartstub] lazy val postcode = {
    def chars(n: Int) = listOfN(n, alphaUpperChar).map(_.mkString)
    def digits(n: Int) = listOfN(n, numChar).map(_.mkString)
    for {
      one <- chars(2)
      two <- digits(2)
      three <- numChar
      four <- chars(2)
    } yield s"$one$two $three$four"
  }
}
