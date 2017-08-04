package hmrc.smartstub

import org.scalatest.prop.Checkers
import org.scalatest._

class SingulariseSpec extends WordSpec with Checkers {
  val resource = this.getClass.getResource("plurals.txt")
  val data = io.Source.fromURL(resource).getLines.filter {
    ! _.startsWith("#")
  }.map{ line =>
    val frags = line.split(" ")
    (frags.last, frags.head)
  }

  "Singularise" should {
    data.foreach { case (plural, singular) =>
      s"""singularise "$plural" to "$singular"""" in {
        assert(Singularise(plural) == singular)
      }
    }
  }
}
