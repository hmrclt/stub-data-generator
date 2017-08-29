package uk.gov.hmrc.smartstub

import scala.language.implicitConversions

import simulacrum._
import org.scalacheck._

@typeclass trait FromLong[A] {
  def size: Long  
  def get(i: Long): Option[A] = i match {
    case low if low < 0 => None
    case high if high > size - 1 => None
    case _ => Some(apply(i))
  }
  
  def apply(i: Long): A = get(i).getOrElse {
    throw new IndexOutOfBoundsException
  }

  def gen: Gen[A] = Gen.choose(0, size - 1).map{apply(_)}
  def arbitrary: Arbitrary[A] = Arbitrary { gen }
}

@typeclass trait ToLong[A] {
  def asLong(i: A): Long
}

@typeclass trait Enumerable[A] extends FromLong[A] with ToLong[A] {

  def head = apply(0L)
  def last = apply(size - 1)

  def succ(i: A): A = apply(asLong(i) + 1)
  def pred(i: A): A = apply(asLong(i) - 1)

  def iterator: Iterator[A] = { 
    val s = size - 1
    new Iterator[A] {
      var pos: A = head
      def hasNext: Boolean = asLong(pos) < { s - 1}
      def next(): A = {pos = succ(pos); pos}
    }
  }

  def imap[B](f: A => B)(invf: B => A): Enumerable[B] = {
    val base = this
    new Enumerable[B] {
      override def get(i: Long) = base.get(i).map(f)
      def size: Long = base.size
      def asLong(i: B): Long = base.asLong(invf(i))
    }
  }
}

object Enumerable {
  object instances {
    implicit val longEnum = new Enumerable[Long] {
      override def get(i: Long): Option[Long] = Some(i).filter{_ >= 0}
      def asLong(i: Long): Long = i
      val size = Long.MaxValue
    }

    implicit val ninoEnum: Enumerable[String] = pattern"ZZ 99 99 99 D"
    implicit val ninoEnumNoSpaces: Enumerable[String] = pattern"ZZ999999D"

    implicit val utrEnum: Enumerable[String] = {
      def checkDigit(in: String): Char = {
        val checkString = "21987654321"
        val weights = List(6, 7, 8, 9, 10, 5, 4, 3, 2)
        val pos = {weights zip in}.map {
          case (weight, char) => weight * char.asDigit
        }.sum % checkString.size
        checkString(pos)
      }

      pattern"999999999".imap{ in =>
        checkDigit(in).toString ++ in
      }( _ match {
        case s if s.head == checkDigit(s.tail) => s.tail
        case _ => throw new IllegalArgumentException
      })
    }

    implicit val sortCodeEnum: Enumerable[String] = pattern"99-99-99"
    implicit val accountNumberEnum: Enumerable[String] = pattern"99999999"

    implicit val empRefEnum: Enumerable[String] = pattern"999/Z999"
  }
}
