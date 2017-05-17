# Mutable Data 



```scala
scala> import Enumerable.instances.ninoEnum
import Enumerable.instances.ninoEnum

scala> val store = Gen.ukAddress.asMutable[String]
store: hmrc.smartstub.PersistentGen[String,List[String]] = org.scalacheck.Gen$$anon$3@7516c2e9

scala> store("NI 31 00 00 A")
res0: List[String] = List(15C, Pettsgrove Avenue, Lerwick, ZE75 8OI)

scala> store("NI 31 00 00 A") = List(
     |   "14 Madeup Avenue", 
     |   "Pseudotown",
     |   "Genericshire",
     |   "AS12 5TR")

scala> store("NI 31 00 00 A")
res2: List[String] = List(14 Madeup Avenue, Pseudotown, Genericshire, AS12 5TR)

scala> store.reset("NI 31 00 00 A")
res3: hmrc.smartstub.PersistentGen[String,List[String]] = org.scalacheck.Gen$$anon$3@7516c2e9

scala> store.get("NI 31 00 00 A")
res4: Option[List[String]] = Some(List(15C, Pettsgrove Avenue, Lerwick, ZE75 8OI))

scala> store -= "NI 31 00 00 A"
res5: store.type = org.scalacheck.Gen$$anon$3@7516c2e9

scala> store.get("NI 31 00 00 A")
res6: Option[List[String]] = None
```
