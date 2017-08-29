package uk.gov.hmrc.smartstub

import org.scalacheck._
import Gen._

trait Pattern extends Any {

  def pattern[T](pattern: Seq[Seq[T]]) = new Enumerable[Seq[T]] {

    private val chars = pattern.reverse.map(_.zipWithIndex.toMap.mapValues{_.toLong})
    private val charsR = chars.map{_.map{_.swap}}
    private val charPermutations = chars.map{_.size.toLong}
    val size = charPermutations.product
    private def maxValue = size - 1
    private val charValues = charPermutations.tails.map{_.product}.toList.tail

    private def zip3[A,B,C](as: Iterable[A], bs: Iterable[B], cs: Iterable[C]): Iterable[(A,B,C)] =
      as.zip(bs).zip(cs).map {
        case ((a,b),c) => (a,b,c)
      }

    def asLong(i: Seq[T]): Long =
      zip3(i.reverse, charValues, chars).map {
        case (c, value, cMap) => cMap(c) * value
      }.sum

    override def get(i: Long): Option[Seq[T]] = i match {
      case low if low < 0 => None
      case high if high > maxValue => None
      case _ => charsR.zip(charValues).foldLeft{(i, List.empty[T])} {
        case ((r, vals), (posChars,posValue)) =>
          (r % posValue, posChars(r / posValue) :: vals)
      } match {
        case (0,x) => Some(x)
        case (r,f) => throw new IllegalStateException(s"Remainder $r with generated $f")
      }
    }      
  }  
}
