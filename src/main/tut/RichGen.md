# Simple Usage 

## Using ScalaCheck to generate data

**1**. **smart-stub-generator** is built upon ScalaCheck Generators, and
works on the same principle. First import the scalacheck components
and use it to randomly generate a persons name - 

```scala
scala> import org.scalacheck._
import org.scalacheck._

scala> val personName = Gen.alphaStr
personName: org.scalacheck.Gen[String] = org.scalacheck.Gen$$anon$3@18e74dcf

scala> personName.sample.foreach(println)
KmegnqzoKdhcrlshwfayeewczwifaxdahqfieoekglawkdgokfwvnvkboydr
```

We have a couple of problems with the test data that is
produced - 

1. The data is not particularly plausible - ScalaCheck is great at
   finding edge cases for testing, but not so good if you want
   plausible data for demonstrating or visualising. In this case the
   persons name, while of the correct type (a String) its value is not 
   likely, even amongst footballers children. 
2. The data is not stable - if you run the same code again you would
   get a different name being produced. 
   
## Procedurally generating data

Computers do not actually generate truly random values (unless you
have a hardware random number generator), but instead use complex
functions to create the appearance of randomness. By controlling the
input into these functions (the seed) we can create something that has
the appearance of randomness but is completely deterministic. 

```scala
scala> import hmrc.smartstub._
import hmrc.smartstub._

scala> val personName = Gen.forename.seeded(1L).get
personName: String = Charlotte
```

## Composing data 

```scala
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
personGenerator: org.scalacheck.Gen[Person] = org.scalacheck.Gen$$anon$3@2c6fa8d7

scala> val person = personGenerator.seeded(1L).get
person: Person = Person(Female,Charlotte Kaur,1965-05-22,List(140, Penny Hedge, Dumfries, DG58 8MG))
```

## Enumerations and Patterns 

### Every pattern is an enumeration - 

```scala
scala> val windowTaxRef = Enumerable.patterned("BZZ-99999C")
windowTaxRef: hmrc.smartstub.Enumerable[String] = hmrc.smartstub.Enumerable$$anon$3@3f3c1e47

scala> windowTaxRef.head
res1: String = AAA-00000A

scala> windowTaxRef.last
res2: String = BZZ-99999C

scala> windowTaxRef.size
res3: Long = 405600000

scala> windowTaxRef.iterator
res4: Iterator[String] = non-empty iterator
```

### Enumerations are bidirectional - 

```scala
scala> val thousandthCustomer = windowTaxRef(1000L)
thousandthCustomer: String = AGT-00000A

scala> windowTaxRef.asLong(thousandthCustomer)
res5: Long = 1000
```

This property enables Enumerations to be used as both the input (seed)
and the value being generated.

### Using enumerations as the target of the generation - 

```scala
scala> case class WindowTaxAccount(
     |   person: Person, 
     |   taxAccountRef: Option[String]
     | )
defined class WindowTaxAccount

scala> val windowTaxGenerator: Gen[WindowTaxAccount] = for { 
     |   person <- personGenerator
     |   ref <- Gen.option(windowTaxRef.gen)
     | } yield WindowTaxAccount(person, ref)
windowTaxGenerator: org.scalacheck.Gen[WindowTaxAccount] = org.scalacheck.Gen$$anon$3@79bbb29b
```

### Using enumerations as the input for the generation - 

```scala
scala> import hmrc.smartstub.Enumerable.instances.ninoEnum
import hmrc.smartstub.Enumerable.instances.ninoEnum

scala> implicitly[Enumerable[String]]
res6: hmrc.smartstub.Enumerable[String] = hmrc.smartstub.Enumerable$$anon$3@765b6141

scala> val ninoNineThousandAndNine = Enumerable[String].apply(9009L)
ninoNineThousandAndNine: String = NI 31 00 00 A

scala> windowTaxGenerator.seeded(ninoNineThousandAndNine).get
res7: WindowTaxAccount = WindowTaxAccount(Person(Male,Carter Moore,1958-03-27,List(77, Nuthampstead Road, Derby, DE70 2AA)),None)
```

## Mutable Data 

```scala
scala> val store = Gen.ukAddress.asMutable[String]
store: hmrc.smartstub.PersistentGen[String,List[String]] = org.scalacheck.Gen$$anon$3@220c4439

scala> store("NI 31 00 00 A")
res8: List[String] = List(15C, Pettsgrove Avenue, Lerwick, ZE75 8OI)

scala> store("NI 31 00 00 A") = List(
     |   "14 Madeup Avenue", 
     |   "Pseudotown",
     |   "Genericshire",
     |   "AS12 5TR")

scala> store("NI 31 00 00 A")
res10: List[String] = List(14 Madeup Avenue, Pseudotown, Genericshire, AS12 5TR)

scala> store.reset("NI 31 00 00 A")
res11: hmrc.smartstub.PersistentGen[String,List[String]] = org.scalacheck.Gen$$anon$3@220c4439

scala> store.get("NI 31 00 00 A")
res12: Option[List[String]] = Some(List(15C, Pettsgrove Avenue, Lerwick, ZE75 8OI))

scala> store -= "NI 31 00 00 A"
res13: store.type = org.scalacheck.Gen$$anon$3@220c4439

scala> store.get("NI 31 00 00 A")
res14: Option[List[String]] = None
```
