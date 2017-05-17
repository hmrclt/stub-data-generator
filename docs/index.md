# The Stub Data Generator

The **stub-data-generator** is a library that helps to rapidly create a microservice stub. That is, a microservice that will return plausible looking data in response to calling a RESTful endpoint. Typically the data is returned in the form of JSON that can be consumed by other microservices.

The data that the library generates is not intended to drive tests. Instead of attempting to produce unusual or edge case data that might stress another service and reveal any flaws, the data produced by the **stub-data-generator **is supposed to look like the sort of thing the service that the stub is mimicking would produce. This is intended for demos, and for to help create services that depend on the data when the source of real data is not available.

This website contains both tutorial and reference material. Questions and comments on the documentation can be sent to andy.dwelly@pontifex.io.

[Starting with the stub data generator.](1-SimpleData.html)

[Generating Other data.](2-OtherData.html)


Creating a stub in scala.

etc.
