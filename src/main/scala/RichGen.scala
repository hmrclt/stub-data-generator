package hmrc.smartstub

import org.scalacheck._
import Gen._

case class RichGen[A](g: Gen[A]) extends AnyVal {
  def seeded[IN](in: IN)(implicit tl: ToLong[IN]) =
    g(Parameters.default, rng.Seed(tl.asLong(in)))

  def asMutable[K](implicit en: Enumerable[K]): PersistentGen[K,A] =
    new PersistentGen(
      g, scala.collection.concurrent.TrieMap.empty[K,Option[A]]
    )

  def iterator[K](implicit en: Enumerable[K]): Iterator[A] =
    en.iterator.flatMap(seeded(_))

  def optFrequency(i: Int): Gen[Option[A]] = 
    Gen.frequency(
      i -> g.map(Some(_)),
      {100 - i} -> const[Option[A]](None)
    )

  def hardlyEver:   Gen[Option[A]] = optFrequency(1)
  def rarely:       Gen[Option[A]] = optFrequency(25)
  def sometimes:    Gen[Option[A]] = optFrequency(50)
  def usually:      Gen[Option[A]] = optFrequency(75)
  def almostAlways: Gen[Option[A]] = optFrequency(99)

}
