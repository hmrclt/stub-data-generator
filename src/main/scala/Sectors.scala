package hmrc.smartstub

import hmrc.smartstub.Loader
import org.scalacheck.Gen
import org.scalacheck.Gen.oneOf

/**
  * Created by matt on 27/06/17.
  */

sealed trait Industry
case object Service extends Industry
case object Raw extends Industry
case object Production extends Industry


trait Sectors extends Any {

  def _sector = Sectors._sector

  def industry: Gen[Industry] = oneOf(Service, Raw, Production)
  def sector(): Gen[String] = for {
    g <- industry
    v <- _sector(g)
  } yield (v)
  def sector(g: Industry): Gen[String] = _sector(g)

}

object Sectors extends Loader {
  lazy val _sector: Map[Industry, Gen[String]] = Map(
    Service -> loadFile("sectors-service-industries.txt"),
    Production -> loadFile("sectors-production-industries.txt"),
    Raw -> loadFile("sectors-raw-materials.txt")
  )

}