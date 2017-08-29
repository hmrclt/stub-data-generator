package hmrc.smartstub

import org.scalacheck.Gen
import org.scalacheck.Gen._
import cats.implicits._
import org.scalacheck.support.cats._
import java.util.NoSuchElementException

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

  val multimap: Map[Int, Map[Seq[A], Gen[A]]] = {
    (for (i <- 1 to windowSize) yield i -> markov(i)).toMap
  }

  private def start:Seq[A] = {
    if (terminus.nonEmpty) {
      terminus
    } else {
      multimap(1).keySet.head
    }
  }

  def sized(numElems: Int): Gen[List[A]] = {
    repeatM(next().map(_.pure[List]), {
      acc: List[A] => next(trimSeed(acc)).map { acc :+ _} 
    }, numElems)
  }

  private def markov(ws: Int): Map[Seq[A], Gen[A]] = {
    // slide over the given data sequence creating a sequence of sub-sequences
    data.view.iterator.sliding(ws + 1).withPartial(false).
      // map the preceding and last items in each sub-sequence as K -> V
      map(x => (x.init, Seq(x.last))).toList.groupBy(_._1).
      // create a Gen to associate with the key that knows the
      // weighted probability of each value
      mapValues{v =>
        frequency(v.flatMap(_._2).groupBy(identity).
          mapValues(_.size).toSeq.map(x => (x._2, const(x._1))): _*)}
  }

  private def trimSeed(seed: Seq[A]): Seq[A] = 
    if (seed.length > windowSize) seed.takeRight(windowSize) else seed

  def next(seed: Seq[A] = start): Gen[A] = {
    val last = trimSeed(seed)
    try {
      multimap(last.size)(last)
    } catch {
      case e: NoSuchElementException =>
        throw new NoSuchElementException(s"$seed not found in original data.")
    }
  }
}
