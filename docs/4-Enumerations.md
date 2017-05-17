# Enumerations and Patterns 




## Every pattern is an enumeration - 

```scala
scala> val windowTaxRef = pattern"BZZ-99999C"
windowTaxRef: hmrc.smartstub.Enumerable[String] = hmrc.smartstub.Enumerable$$anon$3@70a660d5

scala> windowTaxRef.head
res0: String = AAA-00000A

scala> windowTaxRef.last
res1: String = BZZ-99999C

scala> windowTaxRef.size
res2: Long = 405600000

scala> windowTaxRef.iterator
res3: Iterator[String] = non-empty iterator
```

## Enumerations are bidirectional - 

```scala
scala> val thousandthCustomer = windowTaxRef(1000L)
thousandthCustomer: String = AGT-00000A

scala> windowTaxRef.asLong(thousandthCustomer)
res4: Long = 1000
```

This property enables Enumerations to be used as both the input (seed)
and the value being generated.

## Using enumerations as the target of the generation - 

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
windowTaxGenerator: org.scalacheck.Gen[WindowTaxAccount] = org.scalacheck.Gen$$anon$3@6d1b645d
```

## Using enumerations as the input for the generation - 

```scala
scala> import hmrc.smartstub.Enumerable.instances.ninoEnum
import hmrc.smartstub.Enumerable.instances.ninoEnum

scala> implicitly[Enumerable[String]]
res5: hmrc.smartstub.Enumerable[String] = hmrc.smartstub.Enumerable$$anon$3@23f388f7

scala> val ninoNineThousandAndNine = Enumerable[String].apply(9009L)
ninoNineThousandAndNine: String = NI 31 00 00 A

scala> windowTaxGenerator.seeded(ninoNineThousandAndNine).get
res6: WindowTaxAccount = WindowTaxAccount(Person(Male,Carter Moore,1958-03-27,List(77, Nuthampstead Road, Derby, DE70 2AA)),None)
```
