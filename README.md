# Smart Stub Generator (a.k.a. The Font of Infinite Nonsense) 

**smart-stub-generator** is a tool to create data for testing.  It is intended for use within HMRC to help the 'stub' services — microservices that mimic the interfaces of production backend systems. 

The data the generator produces is intended to be plausible but fake eliminating the need to either manually craft test records or take real records and anonymize them. For example when generating names, the names will look real rather than random strings.

The library is built for use within Scala Play 2.5 application controllers, but can be used as easily on the REPL or backed by a RDBMS. 

## SBT Project Setup

**1**. Add the following to `build.sbt`:

```scala
addSbtPlugin("hmrc" %% "stub-data-generator" % "0.1.0")
```

## Documentation
1. [Using ScalaCheck to generate data](docs/1-ScalaCheckPrimer.md)
2. [Procedurally generating data](docs/2-ProceduralGeneration.md)
3. [Composing data](docs/3-ComposingData.md)
4. [Enumerations and Patterns](docs/4-Enumerations.md)
5. [Mutable Data](docs/5-MutatingData.md)
6. [Simple Usage](docs/RichGen.md)
     1	[Generating simple data](docs/1-ScalaCheckPrimer.md)
     2	[Generating simple data](docs/1-SimpleData.md)
     3	[Procedurally generating data](docs/2-ProceduralGeneration.md)
     4	[Composing data](docs/3-ComposingData.md)
     5	[Enumerations and Patterns](docs/4-Enumerations.md)
     6	[Mutable Data](docs/5-MutatingData.md)
     7	[Simple Usage](docs/RichGen.md)
