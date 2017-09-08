package uk.gov.hmrc.smartstub

import org.scalacheck.Gen
import org.scalacheck.Gen._
import cats.implicits._
import org.scalacheck.support.cats._

/*
 * A MarkovChain implementation that will return a plausible/probable
 * next item given a seed sub-sequence from the original data. Ideally
 * the seed should be windowSize length but it will return something
 * based on any seed (including empty) as long as the non-empty seed
 * exists in the original data.
 *
 * If a terminus is supplied it will use this as seed whenever an
 * empty seed is supplied.
 */
class MarkovChain[A](
  val data: Seq[A],
  val windowSize: Int,
  val terminus: Seq[A] = Nil
) {

  private val multimap: Map[Int, Map[Seq[A], Gen[A]]] =
    (1 to windowSize).map(i => i -> markov(i)).toMap

  private val start: Seq[A] =
    terminus.some.filter(_.nonEmpty).getOrElse {
      multimap(1).keySet.head
    }

  def sized(numElems: Int): Gen[Vector[A]] = {
    repeatM[Gen,(Vector[A],Option[A])](
      next().map{a => (a.toVector,a)} , {
        case (acc, l) => l match {
          case Some(_) =>
            next(trimSeed(acc)).map {
              a => (acc ++ a.toVector, a)
            }
          case None => (acc,l)
        }
      },
      numElems
    )
  }.map(_._1)

  private def markov(ws: Int): Map[Seq[A], Gen[A]] = {
    val subsequences: List[Seq[A]] =
      data.view.iterator.sliding(ws + 1).withPartial(false).toList

    val kvsets: Map[Seq[A],List[A]] = subsequences.map(
      x => (x.init, x.last)
    ).groupBy(_._1).mapValues(_.map(_._2))

    kvsets.mapValues{v =>

      val table = v.groupBy(identity).mapValues(_.size).map{
        case (a,b) => (b,const(a))
      }

      frequency(table.toSeq: _*)
    }
  }

  private def trimSeed(seed: Seq[A]): Seq[A] =
    if (seed.length > windowSize) seed.takeRight(windowSize) else seed

  def next(seed: Seq[A] = start): Gen[Option[A]] = {
    val last = trimSeed(seed)
    multimap.get(last.size).flatMap(_.get(last)).sequence
  }

}
