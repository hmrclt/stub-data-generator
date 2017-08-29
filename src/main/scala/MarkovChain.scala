package hmrc.smartstub

import org.scalacheck.Gen
import org.scalacheck.Gen._

/*
A MarkovChain implementation that will return a plausible/probable next item given a seed sub-sequence
from the original data. Ideally the seed should be windowSize length but it will return something based
on any seed (including empty) as long as the non-empty seed exists in the original data.

If a terminus is supplied it will use this as seed whenever an empty seed is supplied.
 */
class MarkovChain[A](val data: Seq[A], val windowSize: Int, val terminus: Seq[A] = Seq.empty[A]) {

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

  private def markov(ws: Int): Map[Seq[A], Gen[A]] = {
    // slide over the given data sequence creating a sequence of sub-sequences
    data.view.iterator.sliding(ws + 1).withPartial(false)
      // map the preceding and last items in each sub-sequence as K -> V
      .map(x => (x.init, Seq(x.last))).toList.groupBy(_._1)
      // create a Gen to associate with the key that knows the weighted probability of each value
      .mapValues(v =>
        frequency(v.flatMap(_._2).groupBy(identity).mapValues(_.size).toSeq.map(x => (x._2, const(x._1))): _*))
  }

  private def trimSeed(seed: Seq[A]): Seq[A] = {
    if (seed.length > windowSize) {
      seed.takeRight(windowSize)
    } else {
      seed
    }
  }

//  def next(seed: Seq[A] = start): A = {
//    val last = trimSeed(seed)
//    try {
//      multimap(last.size)(last).sample.get
//    } catch {
//      case e: java.util.NoSuchElementException => throw new UnknownMarkovSeedException(e, last.mkString(", "))
//    }
//  }

  def next(seed: Seq[A] = start): Gen[A] = {
    val last = trimSeed(seed)
    try {
      multimap(last.size)(last)
    } catch {
      case e: java.util.NoSuchElementException => throw new UnknownMarkovSeedException(e, last.mkString(", "))
    }
  }
}

class UnknownMarkovSeedException(exception: Throwable, seed: String) extends
        RuntimeException(UnknownMarkovSeedException.message(seed), exception)

object UnknownMarkovSeedException {
  def message(seed: String): String = {
    s"$seed not found in original data."
  }
}