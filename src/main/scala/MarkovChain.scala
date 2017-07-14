package hmrc.smartstub

import scala.collection.{immutable, mutable}
import scala.util.Random

trait MarkovChain {

  val map:immutable.Map[String, String]
  val rndm = new Random

  def buildMarkov(input: String):immutable.Map[String, String] = {
    val map = mutable.Map[String, String]().empty

    // todo think about reducing this single char keyed map to a map for " " only
    for (i <- input.toList.indices if i < input.toList.size - 1)
      mapF(map, input(i).toString, input(i + 1).toString)

    for (i <- input.toList.indices if i < input.toList.size - 3)
      mapF(map, input.substring(i, i + 2), input(i + 2).toString)

    def mapF(m:mutable.Map[String, String], k: String, v: String): Unit = {
      m(k) = m.getOrElseUpdate(k, "")
      m(k) += v
    }
    map.toMap
  }

  def next(last: String = " "):String = {
    if(map.keySet.contains(last)) {
      val possibilities = map(last)
      val next = possibilities.charAt(rndm.nextInt(possibilities.length))
      return next.toString
    }
    " "
  }

}
