# Procedurally generating data

Computers do not actually generate truly random values (unless you
have a hardware random number generator), but instead use complex
functions to create the appearance of randomness. By controlling the
input into these functions (the seed) we can create something that has
the appearance of randomness but is completely deterministic. 

```tut
import org.scalacheck._
import uk.gov.uk.gov.hmrc.smartstub._
val personName = Gen.forename.seeded(1L).get
```
