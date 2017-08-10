package hmrc.smartstub


import java.net.URL

import com.sun.xml.internal.bind.v2.TODO
import org.scalacheck.Gen
import org.scalacheck.Gen.oneOf

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

trait Companies extends Any {
  def company: Gen[String] = Companies._company
}

object Companies extends MarkovChain[Char] with Loader {

  val resource: URL = this.getClass.getResource("company-names.txt") // todo split the txt file over lines
  override val data: Seq[Char] = scala.io.Source.fromURL(resource).mkString.toList
  override val windowSize = 2
//  override val map: Map[Seq[Char], Gen[Char]] = markov
//  override val map: Map[Seq[Char], Seq[Char]] = buildGenericMarkov
//  println(map)


//  private def buildFromMarkov = TODO
  // this should call Gen.markov.sample.get for a given length

  private def buildR(length: Int): String = {

    def innerBuild(l: Int, built: mutable.StringBuilder = mutable.StringBuilder.newBuilder): String = {
      if (l <= built.length) {
        built.toString()
      } else {
        if (built.size > windowSize) {
          built.append(next(built.takeRight(windowSize).toString.toList))
        } else {
          built.append(next(built.toString.toList)) // TODO the problem is here, look at stack trace.
        }
        println("built: " + built)
        innerBuild(l, built)
      }
    }
    innerBuild(length)
  }


  def maybeAmpersand(companyName: String):String = companyName match {
    case c if c.split(" ").length == 2 && (random nextInt 10) < 5 => c.replace(" ", " & ")
    case _ => companyName
  }


  // TODO check the correct way to get rid of magic number warnings
  def maybeLtdGroupInt(companyName: String): String = {
    val foo = 1
    val bar = 20
    companyName match {
      case c if random.nextInt(bar) < foo => c.concat(" Ltd.")
      case c if random.nextInt(bar) < foo => c.concat(" Group")
      case c if random.nextInt(bar) < foo => c.concat(" Inc.")
      case _ => companyName
    }
  }

  def removeSingleCharWords(companyName: String): String = {
    companyName.split(" ").filter(_.length > 1).map(_.capitalize).mkString(" ") // todo should capitalise here maybe
  }

  // not convinced we should finesse at all
  def finesse(companyName: String): String = {
    maybeLtdGroupInt(maybeAmpersand(removeSingleCharWords(companyName)))
  }

  private def buildCompanyName: String = {
    val randomLength = 15
//    buildR(5 + random.nextInt(randomLength)).trim
    finesse(buildR(5 + randomLength).trim)
  }

  private def companies: Seq[String] = {
    val lb = new ListBuffer[String]()
    for (i <- 1 until 750) {
      lb += buildCompanyName
    }
    lb.toList
  }

  lazy val _company: Gen[String] = oneOf(companies)
}
