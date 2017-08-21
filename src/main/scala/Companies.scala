package hmrc.smartstub


import java.net.URL

import org.scalacheck.Gen
import org.scalacheck.Gen.oneOf

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.Random

trait Companies extends Any {
  def company: Gen[String] = Companies._company
}

object Companies extends Loader {

  val resource: URL = this.getClass.getResource("company-names.txt")
  val data: Seq[Char] = scala.io.Source.fromURL(resource).mkString.toList
  val windowSize = 3
  val terminus: List[Char] = " ".toList
  val markovChain = new MarkovChain[Char](data, windowSize, terminus)
  lazy val _company: Gen[String] = oneOf(companies)

  private lazy val random = new Random

  private def buildR(length: Int): String = {

    def innerBuild(l: Int, built: mutable.StringBuilder = mutable.StringBuilder.newBuilder): String = {
      if (l <= built.length) {
        built.toString()
      } else {
        built match {
          case a if a.length >= windowSize => built.append(markovChain.next(built.takeRight(windowSize).toString.toList))
          case b if b.nonEmpty => built.append(markovChain.next(built.toString.toList))
          case _ => built.append(markovChain.next())
        }
        innerBuild(l, built)
      }
    }
    innerBuild(length)
  }

  private def maybeAmpersand(companyName: String):String = companyName match {
    case c if c.split(" ").length == 2 && (random nextInt 10) < 5 => c.replace(" ", " & ")
    case _ => companyName
  }

  private def maybeLtdGroupInt(companyName: String): String = {
    val one = 1
    val twenty = 20
    companyName match {
      case c if random.nextInt(twenty) < one => c.concat(" Ltd.")
      case c if random.nextInt(twenty) < one => c.concat(" Group")
      case c if random.nextInt(twenty) < one => c.concat(" Inc.")
      case c if random.nextInt(twenty) < one => c.concat(" Plc.")
      case c if random.nextInt(twenty) < one => c.concat(" Holdings")
      case _ => companyName
    }
  }

  private def removeShortWordsAndCapitalise(companyName: String, minLength: Int = 2): String = {
    companyName.split(" ").filter(_.length > minLength).map(_.capitalize).mkString(" ")
  }

  private def finesse(companyName: String): String = {
    maybeLtdGroupInt(maybeAmpersand(removeShortWordsAndCapitalise(companyName, 3)))
  }

  private def buildCompanyName: String = {
    val randomLength = 15
    finesse(buildR(7 + random.nextInt(randomLength)).trim)
  }

  private def companies: Seq[String] = {
    val lb = new ListBuffer[String]()
    for (i <- 1 until 1000) {
      lb += buildCompanyName
    }
    lb.toList
  }

}
