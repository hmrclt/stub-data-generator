package uk.gov.hmrc.smartstub

import org.scalatest.prop.Checkers
import org.scalacheck.Arbitrary._
import org.scalacheck.Prop._
import org.scalatest.FlatSpec

class EnumerableSpec extends FlatSpec with Checkers {

  "A Nino" should "convert back and from a Long unchanged" in {
    import Enumerable.instances.ninoEnum
    check{(a: Long) =>
      val t = {a % ninoEnum.size}.abs
      t == ninoEnum.apply(t).asLong}
  }
}
