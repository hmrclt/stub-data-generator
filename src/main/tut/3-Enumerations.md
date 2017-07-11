

## Procedurally generated test data

When generating sample data you often have to pick one from a small collection of possible choices. For example, an individual could be either dead or alive. We would not normally expect an individual to be both.

Imagine we wish to represent this in code:

```tut
import org.scalacheck._
import hmrc.smartstub._
import Gen._

sealed trait LifeSign
case object Alive extends LifeSign
case object Dead extends LifeSign
```

Case objects are a common solution to creating an enumeration in Scala.

We can now generate samples:

```tut
def lifesign: Gen[LifeSign] = oneOf(Dead, Alive)

lifesign.sample
lifesign.sample
lifesign.sample
```

The built in ```Gender``` enumeration is an example of this, where Gender can be either ```Male``` or ```Female```. You can sample Gender in the usual way:

```tut
def gender: Gen[Gender] = oneOf(Male, Female)
gender.sample
gender.sample
gender.sample
```

However, It is possible to take another approach to generating data from a collection of possible choices. The **smart-stub-generator** has a class called ```Enumerable``` (nothing to do with enumerations - it means countable in this context). If you create an implicit object of this class that provides ```get``` and ```asLong``` functions you can create data using a different approach.

```tut
implicit val lse = new Enumerable[LifeSign] { 
  override def get(i: Long) = i match { 
    case 0 => Some(Dead)
    case 1 => Some(Alive)
    case _ => None 
  }
  def asLong(ls: LifeSign): Long = ls match { 
    case Dead => 0
    case Alive => 1
  }
  val size = 2
}
```

The lse value provides a ```get``` that takes a long and give a ```Lifesign``` and an asLong that goes in teh other direction. It can be used as follows:

```tut
lse.gen.sample
lse.gen.sample
```

This is obviously more long winded than using the normal approach with ```Gen``` that's been used throughout the examples up to this point. But it does reveal an important underlying fact, for some classes in the system, there is a mapping between longs and objects of the class.

This is no accident. One of the ideas behind the generator is that although data is generated randomly, it should be possible to take a numeric seed and get a particular example of data. In the Lifesign case above, 0 takes you to 'Alive' and 1 to 'Dead'.

The same approach can be taken with patterns.

```tut
val windowTaxRef = pattern"BZZ-99999C"
windowTaxRef.head
windowTaxRef.last
windowTaxRef.size
val thousandthCustomer = windowTaxRef.get(1000L)
windowTaxRef.asLong(thousandthCustomer)
```

…using ```get``` returns an option, but you can also express this in a more succinct but unsafe fashion as:

```tut
val thousandthCustomer = windowTaxRef(1000L)
```

