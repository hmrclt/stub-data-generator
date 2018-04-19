package uk.gov.hmrc.smartstub

import org.scalatest.prop.Checkers
import org.scalatest.FlatSpec

class MarkovChainSpec extends FlatSpec with Checkers {

  "A MarkovChain[String]" should "generate a string" in {
    new MarkovChain(Seq("one", "two", "three", "two"), 2).sized(3)
  }
}
