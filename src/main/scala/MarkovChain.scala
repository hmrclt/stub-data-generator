package hmrc.smartstub

import org.scalacheck.Gen
import org.scalacheck.Gen._

import scala.collection.{immutable, mutable}
import scala.util.Random

abstract class MarkovChain[A] {

  val data: Seq[A]
  val windowSize: Int
//  val map: Map[Seq[A], Gen[A]]
//  val map: immutable.Map[Seq[A], Seq[A]]
//  val multimap = Map[Int, Map[Seq[A], Gen[A]]]


  lazy val random = new Random

  def m: Map[Int, Map[Seq[A], Gen[A]]] = {
    var m = Map[Int, Map[Seq[A], Gen[A]]]().empty
    for (i  <- 1 to windowSize) {
      m += (i -> markov(i))
    }
    m
  }

  def markov: Map[Seq[A], Gen[A]] = {
    data.view.iterator.sliding(windowSize + 1).withPartial(false)
      .map(x => (x.init, Seq(x.last))).toList.groupBy(_._1)
      .mapValues(v => frequency(v.flatMap(_._2)
        .groupBy(identity).mapValues(_.size).toSeq.map(x => (x._2, const(x._1))): _*))
  }

  def markov(ws: Int): Map[Seq[A], Gen[A]] = {
    data.view.iterator.sliding(ws + 1).withPartial(false)
      .map(x => (x.init, Seq(x.last))).toList.groupBy(_._1)
      .mapValues(v => frequency(v.flatMap(_._2)
        .groupBy(identity).mapValues(_.size).toSeq.map(x => (x._2, const(x._1))): _*))
  }



  def next(last: Seq[A] = m(1).keySet.head): A = {
    println("last: " + last)
    println("head: " + m(last.size).keySet.head)
//    println("map: " + map)
    val gen: Gen[A] = m(last.size)(last)
    gen.sample.get
  }

  // replace the map value duplicates with weights and don't use a sequence as doesn't need to be sequential
  def buildGenericMarkov: immutable.Map[Seq[A], Seq[A]] = {

    // map version
    //    data.iterator.sliding(windowSize + 1).withPartial(false)
    //      .map(x => (x.init, Seq(x.last))).toList.groupBy(_._1).map { case (k,v) => (k,v.flatMap(_._2)) }

    // mapValues version
    data.view.iterator.sliding(windowSize + 1).withPartial(false)
      .map(x => (x.init, Seq(x.last))).toList.groupBy(_._1).mapValues(v => v.flatMap(_._2))

  }

  //  def markovGen: Map[Seq[A], Seq[(A, Int)]] = {
  //    data.view.iterator.sliding(windowSize + 1).withPartial(false)
  //      .map(x => (x.init, Seq(x.last))).toList.groupBy(_._1).mapValues(v => v.flatMap(_._2).groupBy(identity).mapValues(_.size).toSeq)
  //  }

  //  def markovGen: Map[Seq[A], Seq[(Int, A)]] = {
  //    data.view.iterator.sliding(windowSize + 1).withPartial(false)
  //      .map(x => (x.init, Seq(x.last))).toList.groupBy(_._1)
  //      .mapValues(v => v.flatMap(_._2).groupBy(identity).mapValues(_.size).toSeq.map(_.swap))
  //  }

  // need to return a Gen with const and frequency

//  def next(last: Seq[A] = map.keySet.head): Seq[A] = {
//    if (map.keySet.contains(last)) {
//      val possibilities = map(last)
//      Seq(possibilities(random.nextInt(possibilities.length)))
//    } else {
//      next()
//    }
//  }

}

