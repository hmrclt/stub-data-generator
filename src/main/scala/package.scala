package hmrc

import org.scalacheck._
import scala.language.implicitConversions
import cats.functor.Invariant

package object smartstub
    extends Enumerable.ToEnumerableOps
    with ToLong.ToToLongOps
    with FromLong.ToFromLongOps
{
  implicit def genToRich[A](g: Gen[A]): RichGen[A] = RichGen(g)

  implicit class AdvGen(
    val g: Gen.type
  ) extends AnyVal with Loader
      with Names
      with Addresses
      with Temporal
      with Pattern
      with Sectors
  {
    def boolean: Gen[Boolean] = Gen.oneOf(true, false)
  }


  implicit val longEnum = Enumerable.instances.longEnum

  implicit class PatternContext(val sc: StringContext) extends AnyVal {
    def pattern(i: Any*): Enumerable[String] = Gen.pattern(
      sc.parts.head.map {
        case d if d.isDigit => '0' to d
        case u if u.isUpper => 'A' to u
        case l if l.isLower => 'a' to l
        case x => Seq(x)
      }).imap(_.mkString)(s => {s: Seq[Char]})
  }

  implicit val enumInvariant: Invariant[Enumerable] = new Invariant[Enumerable] {
    def imap[A, B](fa: Enumerable[A])(f: A => B)(finv: B => A): Enumerable[B] =
      fa.imap(f)(finv)
  }

}
