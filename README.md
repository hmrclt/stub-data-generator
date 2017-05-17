# Smart Stub Generator (a.k.a. The Font of Infinite Nonsense) 

**smart-stub-generator** is a tool to create data for testing.  It is intended for use within HMRC to help the 'stub' services — microservices that mimic the interfaces of production backend systems. 

The data the generator produces is intended to be plausible but fake eliminating the need to either manually craft test records or take real records and anonymize them. For example when generating names, the names will look real rather than random strings.

The library is built for use within Scala Play 2.5 application controllers, but can be used as easily on the REPL or backed by a RDBMS. 

## SBT Project Setup

**1**. Add the following to `build.sbt`:

```scala
addSbtPlugin("hmrc" %% "stub-data-generator" % "0.1.0")
```

## Documentation     1	[Generating simple data](target/post-tut/1-SimpleData.md)
     2	[Generating other kinds of data](target/post-tut/2-OtherData.md)
     3	[Composing data](target/post-tut/3-ComposingData.md)
     4	[Enumerations and Patterns](target/post-tut/4-Enumerations.md)
     5	[Mutable Data](target/post-tut/5-MutatingData.md)
     6	[Simple Usage](target/post-tut/RichGen.md)
