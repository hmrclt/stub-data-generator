# Generating other kinds of data

The **smart-stub-generator** can produce a variety of other kinds of data including gendered names, email addresses, dates, uk addresses, postcodes,  and a very wide variety of reference numbers, NINOs and similar strings using the powerful concept of a pattern. These elements can then be combined to produce composite data structures such as case class representing a person with a forname, surname, date of birth, and a NINO.

## Gendered names

In the previous section we saw that it was possible to produce sensible looking names; but sometimes we need a plausible example of someone of a particular gender. For example, we would expect all mothers to be female. We can restrict forenames by genender in order to achieve this:

```tut
import org.scalacheck._
import hmrc.smartstub._

val boy = Gen.forename(Male)
val girl = Gen.forename(Female)
s"${boy.sample.get} likes ${girl.sample.get}"
s"${girl.sample.get} likes ${boy.sample.get}"
```

## Email addresses

Email addresses can be produced on demand:

```tut
import org.scalacheck._
import hmrc.smartstub._

//To be supplied
```

Sometimes it is useful to be able to relate the email to a name. In order to do this, you can include elements from names that _might_ be included

```tut
import org.scalacheck._
import hmrc.smartstub._

val firstNames = Gen.forename
val lastNames = Gen.surname

//get a single first and last name
val f = firstNames.sample.get
val l = lastNames.sample.get

//To be supplied
//generate multiple emails based on that first and last name
//val emails = Gen.email(f, l)
//emails.sample.get
//emails.sample.get
```

## Dates

The dates we deal with are nearly always from the past; the dates generated by default by the **smart-stub-generator **extend from the 1st of January 1970 to the 31st of December 2001. This is intended to be suitable as a basic set of birthdates.

```tut
import org.scalacheck._
import hmrc.smartstub._

val dates = Gen.date
dates.sample.foreach{println}
```

For dates in a wider range, simply supply the beginning and end year - these are inclusive so that if the firts year is 1935 and the end year is 2018, dates in either 1935 or 2018 could be generated.

```tut
import org.scalacheck._
import hmrc.smartstub._

val dates = Gen.date(1935, 2018)
dates.sample.foreach{println}
```

## UK Addresses

The addresses are currently uk personal addresses. The ```sample``` method has a type of ```Option[List[String]]```, so demontrating it needs a ```get``` and a ```map```.

```tut
import org.scalacheck._
import hmrc.smartstub._

val addresses = Gen.ukAddress
addresses.sample.get.map{println}
```

## Postcodes

It is sometimes useful to be able to produce a postcode directly. The postcode generator can be used for this:

```tut
import org.scalacheck._
import hmrc.smartstub._

val postcodes = Gen.postcode
postcodes.sample.get
```

## Patterns

Business data often uses reference numbers for identification and other purposes. The **stub-data-generator** can generate arbitrary string patterns for this. It is also capable of generating other types if necessary.

Patterns generate a finite list of data so they are generally accessed using the usual access methods such as  ```head```, ```tail```, or accessed via an index. As usual if an index is greater than or equal to the ```size``` an exception will be thrown.

You can get a random element from the pattern by creating a generator with the ```gen``` method and taking a sample.

```tut
import org.scalacheck._
import hmrc.smartstub._

val patterns = pattern"Z Z"
patterns.head
patterns(35L)
patterns.gen.sample
```

Patterns are present in the library as string operators, similar to Saca's ```s"…"``` and ```r"…"``` The characters of the string generate the patterns as follows:

1. A lower case letter will create patterns from 'a' up to and including the specified letter. So 'c' will generate 'a' or 'b' or 'c'.
2. An upper case letter will create patterns from 'A' up to and including the specified letter. So 'D' will generate 'A' or 'B' or 'C' or 'D'.
3. A digit will generate a number from '0' up to and including the digit itself. So, '3' will generate '1' or '2' or '3'.
4. Any other character will appear unchanged, so putting a space in the string will always leave that part of the pattern with a space in it.

It is possible to create other kinds of data from the Pattern class that the library contails. Patterns are just sequences of sequences, so almost anything could be created, a simple example would be a pattern that generates even numbers between 1 and 100.

```tut
import org.scalacheck._
import hmrc.smartstub._

val patterns = Gen.pattern(Seq({0 to 100}.filter(_ % 2 == 0)))
patterns.gen.sample.get.head
```



## Composite data

Usually we are dealing with collections of very simple data to produce objects representing for example, businesses, people, organizations, and so on. In Scala, such collections are almost always represented by case classes. We can easily produce plausible examples of data in the case class form because the generators can be incuded in Scala's for comprehensions (e.g. they are monads.). 

Note how in the following example, the result of the forename generator is dependant on the result of the gender generator:

```tut
import org.scalacheck._
import hmrc.smartstub._

case class Person(
gender: Gender,
name: String,
dateOfBirth: java.time.LocalDate,
address: List[String])

val people: Gen[Person] = for {
gender <- Gen.gender
fname <- Gen.forename(gender)
sname <- Gen.surname
dob <- Gen.date(1935, 2000)
address <- Gen.ukAddress
} yield Person(gender, s"$fname $sname", dob, address)

people.sample.get
```


