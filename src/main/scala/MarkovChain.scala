package hmrc.smartstub

import org.scalacheck.Gen
import org.scalacheck.Gen._

class MarkovChain[A](val data: Seq[A], val windowSize: Int, val terminus: Seq[A] = Seq.empty[A]) {

  private val multimap: Map[Int, Map[Seq[A], Gen[A]]] = mapOfMaps

  private def mapOfMaps: Map[Int, Map[Seq[A], Gen[A]]] = {
    (for (i <- 1 to windowSize) yield i -> markov(i)).toMap
  }

  private def start:Seq[A] = {
    terminus match {
      case a if a.isEmpty => multimap(1).keySet.head
      case _ => terminus
    }
  }

  private def markov(ws: Int): Map[Seq[A], Gen[A]] = {
    data.view.iterator.sliding(ws + 1).withPartial(false)
      .map(x => (x.init, Seq(x.last))).toList.groupBy(_._1)
      .mapValues(v =>
        frequency(v.flatMap(_._2).groupBy(identity).mapValues(_.size).toSeq.map(x => (x._2, const(x._1))): _*))
  }

  def next(last: Seq[A] = start): A = {
    multimap(last.size)(last).sample.get
  }

}

