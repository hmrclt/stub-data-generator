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

```scala
scala> import org.scalacheck._
import org.scalacheck._

scala> val personName = Gen.alphaStr
personName: org.scalacheck.Gen[String] = org.scalacheck.Gen$$anon$3@6b86f8f8

scala> personName.sample.foreach(println)
gavIxhibldathisycubilqbznmklcxnenqPGUjgqpudIdrqbytxzhqzlhbbcmfkdzQbvdryvRnautqbvvgdydpxprudgasuqng
```

This is ideal for producing tests with the intent of stressing our code since the strings that are produced are the kind of names that we would not normally use. They exercise the code in the presence of really unusual names (for example names hundreds of characters long, or names with punctuation in them).

From the perspective of rapidly creating a stub that has to produce _plausible_ data, ScalaCheck is limited not ideal though. We want to construct names that look like they might represent a real person so that we can either demonstrate our work or visualize it.

We would also like to be able to recreate data on demand. ScalaCheck will produce a different alphanumeric string each time it is asked to give a sample. So if you were to restart sbt and create another personName you would get an entirely different string:

```scala
scala> import org.scalacheck._
import org.scalacheck._

scala> val personNames = Gen.alphaStr
personNames: org.scalacheck.Gen[String] = org.scalacheck.Gen$$anon$3@1c109aa5

scala> personNames.sample.foreach(println)
fmqoQvttKegdIbdOQepbWsjrwWzREGmVeqiwwluakhzscyyiohxfwoowQiflixUbnyxtwAyslmuwkrbilaOdjx
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

```scala
scala> import org.scalacheck._
import org.scalacheck._

scala> import hmrc.smartstub._
import hmrc.smartstub._

scala> val names = Gen.forename
names: org.scalacheck.Gen[String] = org.scalacheck.Gen$$anon$3@3e8d8512

scala> names.sample.foreach(println)
Hannah

scala> names.sample.foreach(println)
David

scala> names.sample.foreach(println)
Nathan
```

We can generate surnames in a similar fashion:

```scala
scala> import org.scalacheck._
import org.scalacheck._

scala> import hmrc.smartstub._
import hmrc.smartstub._

scala> val lastnames = Gen.surname
lastnames: org.scalacheck.Gen[String] = org.scalacheck.Gen$$anon$1@38044dec

scala> lastnames.sample.foreach{println}
Shannon

scala> lastnames.sample.foreach{println}
Slade

scala> lastnames.sample.foreach{println}
Brook
```

These meet the requirement that the data that is generated is plausible, but using this method the data is not repeatable. If the console is restarted and another surname is generated:

```scala
scala> import org.scalacheck._
import org.scalacheck._

scala> import hmrc.smartstub._
import hmrc.smartstub._

scala> val lastnames = Gen.surname
lastnames: org.scalacheck.Gen[String] = org.scalacheck.Gen$$anon$1@38044dec

scala> lastnames.sample.foreach{println}
Palmer
```

The surnames generated will be different.

How can we ensure that the same names are generated on demand? We can use the ``seeded`` method rather than the ```sample``` method:

```scala
scala> import org.scalacheck._
import org.scalacheck._

scala> import hmrc.smartstub._
import hmrc.smartstub._

scala> val names = Gen.forename
names: org.scalacheck.Gen[String] = org.scalacheck.Gen$$anon$3@4cab1eb0

scala> names.seeded(1L).foreach{println}
Charlotte

scala> names.seeded(2L).foreach{println}
Isaiah

scala> names.seeded(12874638L).foreach{println}
Noah
```

Even if the console is restarted, ``seeded(2L)``  will always return "Isaiha":

```scala
scala> import org.scalacheck._
import org.scalacheck._

scala> import hmrc.smartstub._
import hmrc.smartstub._

scala> val names = Gen.forename
names: org.scalacheck.Gen[String] = org.scalacheck.Gen$$anon$3@dcc7371

scala> names.seeded(2L).foreach{println}
Isaiah
```

This leads to an important difference when generating a collection of names. If we want each one to be random, the sample method has to be called for each one, and each time it will give an arbitrary answer:

```scala
scala> import org.scalacheck._
import org.scalacheck._

scala> import hmrc.smartstub._
import hmrc.smartstub._

scala> val randomNames = (1 to 5).map{x => names.sample.get}
randomNames: scala.collection.immutable.IndexedSeq[String] = Vector(Layla, Sebastian, Abigail, Aaliyah, Muhammad)

scala> val moreRandomNames = (1 to 5).map{x => names.sample.get}
moreRandomNames: scala.collection.immutable.IndexedSeq[String] = Vector(Adeline, Caden, Anna, Elijah, Eli)
```

But if we want to generate the same collection of names each time, we have to use ```seeded``` with the same inputs:

```scala
import org.scalacheck._
import hmrc.smartstub._

val seededNames = (1 to 5).map{x => name.seeded(x.toLong).get}
val moreSeededNames = (1 to 5).map{x => name.seeded(x.toLong).get}
```

Note that the integer ```x``` has to be explicitly converted into a long to make this work.

There are a variety of additional data generators available covering emails, national insurance numbers (NINOs), uk addresses, and many more. The library also allows strings that meet arbitrary patterns to be created. These are covered in the following section.



