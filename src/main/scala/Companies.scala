package hmrc.smartstub

import org.scalacheck.Gen

trait Companies extends Any {
  def company: Gen[String] = Companies.company
}

object Companies extends Loader {

  val markovChain = new MarkovChain[Char](
    data = scala.io.Source.fromURL(
      getClass.getResource("company-names.txt")
    ).mkString.toList,
    windowSize = 3,
    terminus = " ".toList)

  def company: Gen[String] = for {
    size <- Gen.choose(5, 20)
    base <- companyBase(size)
    suffix <- Gen.oneOf("Ltd.,Group,Inc.,Plc.,Holdings".split(",")).sometimes
  } yield {
    List(Some(base), suffix).flatten.mkString(" ")
  }

  private def companyBase(length: Int): Gen[String] =
    markovChain.sized(length).map { x => removeShortWordsAndCapitalise(x.mkString) } 

  private def removeShortWordsAndCapitalise(companyName: String, minLength: Int = 2): String = {
    companyName.split(" ").filter(_.length > minLength).map(_.capitalize).mkString(" ")
  }
}
