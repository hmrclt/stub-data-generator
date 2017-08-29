# Composing data 

```tut
import org.scalacheck._
import uk.gov.uk.gov.hmrc.smartstub._

case class Person(
  gender: Gender, 
  name: String, 
  dateOfBirth: java.time.LocalDate, 
  address: List[String]
)

val personGenerator: Gen[Person] = for { 
  gender <- Gen.gender
  fname <- Gen.forename(gender)
  sname <- Gen.surname
  dob <- Gen.date(1940,2010)
  address <- Gen.ukAddress
} yield Person(gender, s"$fname $sname", dob, address)

val person = personGenerator.seeded(1L).get
```
