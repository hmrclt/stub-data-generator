# Composing data 

```scala
scala> import org.scalacheck._
import org.scalacheck._

scala> import hmrc.smartstub._
import hmrc.smartstub._

scala> case class Person(
     |   gender: Gender, 
     |   name: String, 
     |   dateOfBirth: java.time.LocalDate, 
     |   address: List[String]
     | )
defined class Person

scala> val personGenerator: Gen[Person] = for { 
     |   gender <- Gen.gender
     |   fname <- Gen.forename(gender)
     |   sname <- Gen.surname
     |   dob <- Gen.date(1940,2010)
     |   address <- Gen.ukAddress
     | } yield Person(gender, s"$fname $sname", dob, address)
personGenerator: org.scalacheck.Gen[Person] = org.scalacheck.Gen$$anon$3@730bb926

scala> val person = personGenerator.seeded(1L).get
person: Person = Person(Female,Charlotte Kaur,1965-05-22,List(140, Penny Hedge, Dumfries, DG58 8MG))
```
