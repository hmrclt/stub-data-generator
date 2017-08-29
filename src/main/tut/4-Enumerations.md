# Enumerations and Patterns 

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

## Every pattern is an enumeration - 

```tut
val windowTaxRef = pattern"BZZ-99999C"
windowTaxRef.head
windowTaxRef.last
windowTaxRef.size
windowTaxRef.iterator
```

## Enumerations are bidirectional - 

```tut
val thousandthCustomer = windowTaxRef(1000L)
windowTaxRef.asLong(thousandthCustomer)
```

This property enables Enumerations to be used as both the input (seed)
and the value being generated.

## Using enumerations as the target of the generation - 

```tut
case class WindowTaxAccount(
  person: Person, 
  taxAccountRef: Option[String]
)

val windowTaxGenerator: Gen[WindowTaxAccount] = for { 
  person <- personGenerator
  ref <- Gen.option(windowTaxRef.gen)
} yield WindowTaxAccount(person, ref)
```

## Using enumerations as the input for the generation - 

```tut
import uk.gov.uk.gov.hmrc.smartstub.Enumerable.instances.ninoEnum
implicitly[Enumerable[String]]
val ninoNineThousandAndNine = Enumerable[String].apply(9009L)
windowTaxGenerator.seeded(ninoNineThousandAndNine).get
```
