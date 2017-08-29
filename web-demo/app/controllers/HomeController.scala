package controllers

import javax.inject._
import play.api._
import play.api.mvc._
import org.scalacheck._
import uk.gov.hmrc.smartstub._
import AutoGen._

sealed trait Computer
case object MacMachine extends Computer
case object LinuxMachine extends Computer
case object WindowsMachine extends Computer

case class TaxPayer(
  gender: Gender,
  forename: String,
  middlename: Option[String],
  surname: String,
  job: Option[String],
  address: String,
  utr: String,
  computers: List[Computer]
) {
  def rebateDue: Int = Math.max(0,
    if (job == Some("Programmer") && computers.nonEmpty) {
      10000 - 1000 * computers.count(_ == MacMachine)
    } else {
      0
    })
}


@Singleton
class HomeController @Inject() extends Controller {

  implicit val nino = pattern"ZZ999999D"

  val store = {
    for {
      gender <- Gen.gender
      forename <- Gen.forename(gender)
      middlename <- Gen.forename(gender).usually.map{_.filter{_ != forename}}
      surname <- Gen.surname
      address <- Gen.ukAddress.map{_.mkString(", ")}
      utr <- Enumerable.instances.utrEnum.gen
      job <- Gen.oneOf("Programmer","Sysadmin", "Scrum Master", "UX", "QA").usually
      computerNum <- Gen.choose(0,5)
      computers <- Gen.listOfN(computerNum, Gen.oneOf(MacMachine, LinuxMachine, WindowsMachine))
    } yield {
      TaxPayer(gender, forename, middlename, surname, job, address, utr, computers)
    }
  }.asMutable[String]

  def index(page: Long) = Action { implicit request =>
    val start = (page-1) * 12
    val end = start + 12
    val resultsPage = {start to end}.map{x => (nino(x), store(nino(x)))}
    val numPages = nino.size / 20
    Ok(views.html.index(page, numPages, resultsPage))
  }

  def user(nino: String, page: Long) = Action { implicit request =>
    Ok(views.html.user(store(nino), nino, page))
  }

}
