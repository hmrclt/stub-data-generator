package hmrc.smartstub

import org.scalacheck.Gen

/**
  * Created by matt on 27/06/17.
  */

trait Sectors extends Any {

  def sector: Gen[String] = Sectors._sector

  def industry(): Gen[String] = Sectors._industry

  def industry(get: String): Gen[String] = {
    Sectors._industry(get)
  }

  def job(sector: String, industry: String): Gen[String] = {
    Sectors._job(sector, industry)
  }

  def job: Gen[String] = {
    Sectors._job
  }

  // TODO this is redundant unless I can find a way to pass parameters from AutoGen
  def industryForJob(job: String): Gen[String] = {
    Sectors._industryForJob(job)
  }
}

object Sectors extends Loader {

  lazy val _sector: Gen[String] = hf(0)
  lazy val _industry: Gen[String] = hf(1)
  lazy val _job: Gen[String] = hf(2)
  val hf = HierarchicalFile("sectors-hierarchical.txt")

  def _industry(sector: String): Gen[String] = {
    hf(sector)
  }

  def _job(sector: String, industry: String): Gen[String] = {
    hf(sector, industry)
  }

  def _industryForJob(job: String): Gen[String] = {
    hf(Backwards, job)
  }
}
