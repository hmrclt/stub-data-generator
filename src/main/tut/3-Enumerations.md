## Enumerations

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

We can now generate samples

```tut
def lifesign: Gen[LifeSign] = oneOf(Dead, Alive)

lifesign.sample
lifesign.sample
lifesign.sample
```



