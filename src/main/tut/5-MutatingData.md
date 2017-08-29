# Mutable Data 
```tut:invisible
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
```

```tut

import Enumerable.instances.ninoEnum

val store = Gen.ukAddress.asMutable[String]
store("NI 31 00 00 A")
store("NI 31 00 00 A") = List(
  "14 Madeup Avenue", 
  "Pseudotown",
  "Genericshire",
  "AS12 5TR")
  
store("NI 31 00 00 A")
store.reset("NI 31 00 00 A")
store.get("NI 31 00 00 A")
store -= "NI 31 00 00 A"
store.get("NI 31 00 00 A")
```
