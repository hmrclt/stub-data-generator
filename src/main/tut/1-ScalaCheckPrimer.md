# Generating simple data

The **smart-stub-generator** works by adding functionality to ScalaCheck and in in order to use the stub generator, you need to have an idea of how ScalaCheck itself works. The ScalaCheck library is not available in the default library and the easiest way to manage the dependancy is to create a directory with a simple build.sbt file in it that brings it in:

```sh
$ mkdir stubs
$ cd stubs
$ echo 'libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.13.4"' > build.sbt
$ sbt console
scala>
```

You will now be able to start using ScalaCheck. For example: 

```tut
import org.scalacheck._

val personName = Gen.alphaStr
personName.sample.foreach(println)
```

This is ideal for producing tests with the intent of stressing our code since the strings that are produced are the kind of names that we would not normally use. They exercise the code in the presence of really unusual names (for example names hundreds of characters long, or names with punctuation in them).

From the perspective of rapidly creating a stub that has to produce _plausible_ data, ScalaCheck is limited not ideal though. We want to construct names that look like they might represent a real person so that we can either demonstrate our work or visualize it.

We would also like to be able to recreate data on demand. ScalaCheck will produce a different alphanumeric string each time it is asked to give a sample. So if you were to restart sbt and create another personName you would get an entirely different string:

```tut
import org.scalacheck._

val personName = Gen.alphaStr
personName.sample.foreach(println)
```

The stub generator addresses both of these issues. In order to try it, you need to add the library dependancy to the build.sbt file:

```sh
$ echo 'libraryDependencies += "hmrc" %% "stub-data-generator" % "0.1.0"' >> build.sbt
```

(Note the use of the ```>>``` in this command, it appends text to a file.) 

Alternatively, open the build.sbt in the text editor of your choice and add:

```scala
libraryDependencies += "hmrc" %% "stub-data-generator" % "0.1.0"
```

Now you can use the ```sbt console``` command to work with the stub generator. We can use the new ```Gen.forename``` method to create plausible first names.

```tut
import org.scalacheck._
import uk.gov.uk.gov.hmrc.smartstub._

val name = Gen.forename
name.sample.foreach(println)
name.sample.foreach(println)
name.sample.foreach(println)
```

We can generate surnames in a similar fashion:

```tut
import org.scalacheck._
import uk.gov.uk.gov.hmrc.smartstub._

val lastname = Gen.surname
lastname.sample.foreach{println}
lastname.sample.foreach{println}
lastname.sample.foreach{println}
```

These meet the requirement that the data that is generated is plausible, but using this method the data is not repeatable. If the console is restarted and another surname is generated:

```tut
import org.scalacheck._
import uk.gov.uk.gov.hmrc.smartstub._

val lastname = Gen.surname
lastname.sample.foreach{println}
```

The surnames generated will be different.

How can we ensure that the same names are generated on demand? We can use the ``seeded`` method rather than the ```sample``` method:

```tut
import org.scalacheck._
import uk.gov.uk.gov.hmrc.smartstub._

val name = Gen.forename
name.seeded(1L).foreach{println}
name.seeded(2L).foreach{println}
name.seeded(12874638L).foreach{println}
```

Even if the console is restarted, ``seeded(2L)``  will always return "Isaiha":

```tut
import org.scalacheck._
import uk.gov.uk.gov.hmrc.smartstub._

val name = Gen.forename
name.seeded(2L).foreach{println}
```

This leads to an important difference when generating a collection of names. If we want each one to be random, the sample method has to be called for each one, and each time it will give an arbitrary answer:

```tut
import org.scalacheck._
import uk.gov.uk.gov.hmrc.smartstub._

val randomNames = (1 to 5).map{x => name.sample.get}
val moreRandomNames = (1 to 5).map{x => name.sample.get}
```

But if we want to generate the same collection of names each time, we have to use ```seeded``` with the same inputs:

```scala
import org.scalacheck._
import uk.gov.uk.gov.hmrc.smartstub._

val seededNames = (1 to 5).map{x => name.seeded(x.toLong).get}
val moreSeededNames = (1 to 5).map{x => name.seeded(x.toLong).get}
```

Note that the integer ```x``` has to be explicitly converted into a long to make this work.

There are a variety of additional data generators available covering emails, national insurance numbers (NINOs), uk addresses, and many more. The library also allows strings that meet arbitrary patterns to be created. These are covered in the following section.



