package uk.gov.hmrc.smartstub

import org.scalacheck._

import shapeless._
import shapeless.labelled._
import shapeless.ops.nat.ToInt

import java.time.LocalDate

object AutoGen extends LowPriorityGenProviderInstances {

  trait GenProvider[A] {
    val gen: Gen[A]
  }

  def apply[A](implicit e: GenProvider[A]): Gen[A] = e.gen

  // instance constructor
  def instance[A](f: Gen[A]): GenProvider[A] = new GenProvider[A] {
    override val gen: Gen[A] = f
  }

  implicit def providerUnnamed[A](implicit g: GenProvider[A]): String ⇒ GenProvider[A] = _ ⇒ g

  implicit def providerString: GenProvider[String] = providerStringNamed("")

  // Named types
  implicit def providerSeqNamed[A](s: String)(implicit inner: String ⇒ GenProvider[A]): GenProvider[Seq[A]] =
    instance(Gen.listOf(inner(s).gen))

  implicit def providerSetNamed[A](s: String)(implicit inner: String ⇒ GenProvider[A]): GenProvider[Set[A]] =
    instance(Gen.listOf(inner(s).gen).map(_.toSet))

  implicit def providerVectorNamed[A](s: String)(implicit inner: String ⇒ GenProvider[A]): GenProvider[Vector[A]] =
    instance(Gen.listOf(inner(s).gen).map(l ⇒l.toVector))

  implicit def providerOptionNamed[A](s: String)(implicit inner: String ⇒ GenProvider[A]): GenProvider[Option[A]] =
    instance(Gen.option(inner(s).gen))

  implicit def providerIntNamed(s: String): GenProvider[Int] = instance ({
    s.toLowerCase match {
      case "age" => Gen.choose(1, 80)
      case _ => Gen.choose(1, 1000)
    }
  })

  implicit def providerStringNamed(s: String): GenProvider[String] = instance ({
    s.toLowerCase match {
      case "forename" | "firstname" => Gen.forename
      case "surname" | "lastname" | "familyname" => Gen.surname
      case x if x.toLowerCase.contains("address") =>
        Gen.ukAddress.map{_.mkString(", ")}
      case "gender" | "sex" => Gen.oneOf("male", "female")
      case "nino" => Enumerable.instances.ninoEnum.gen
      case "utr" => Enumerable.instances.utrEnum.gen
      case "company" => Gen.company
      case _ => Gen.alphaStr
    }
  })

  implicit def providerLocalDate(s: String): GenProvider[LocalDate] = instance({
    s.toLowerCase match {
      case "dateofbirth" | "dob" | "birthdate" | "boron" | "birthday" ⇒ Gen.date(LocalDate.of(1900,1,1), LocalDate.now())
      case _                                                          ⇒ Gen.date
    }
  })

  implicit def providerBooleanNamed(s: String): GenProvider[Boolean] =
    instance(Gen.oneOf(true,false))

  // generic instance

  implicit def providerGeneric[A, H, T]
  (implicit
   generic: LabelledGeneric.Aux[A,T],
   hGenProvider: Lazy[GenProvider[T]]
  ): GenProvider[A] =
    instance(hGenProvider.value.gen.map(generic.from))

  // HList instances

  implicit val providerHNil: GenProvider[HNil] = instance(Gen.const(HNil))

  implicit def providerHCons[K <: Symbol, H, T <: HList]
  (implicit
   witness: Witness.Aux[K],
   hGenProvider: Lazy[String ⇒ GenProvider[H]],
   tGenProvider: Lazy[GenProvider[T]]
  ): GenProvider[FieldType[K,H] :: T] = instance(
    hGenProvider.value(witness.value.name).gen.flatMap(f ⇒
      tGenProvider.value.gen.map{ t ⇒
        field[K](f) :: t
      }
    )
  )

  // Coproduct instances

  implicit def providerCNil: GenProvider[CNil] =
    instance(Gen.delay(throw new Exception("Oh no - CNil!")))

  implicit def providerCCons[K <: Symbol, H, T <: Coproduct, L <: Nat]
  (implicit
   witness: Witness.Aux[K],
   hGenProvider: Lazy[String ⇒ GenProvider[H]],
   tGenProvider: Lazy[GenProvider[T]],
   l: shapeless.ops.coproduct.Length.Aux[H :+: T, L],
   i: ToInt[L]
  ): GenProvider[FieldType[K,H] :+: T] = {
    val headGenerator = hGenProvider.value(witness.value.name).gen.map(h ⇒ Inl(field[K](h)))

    if(i() == 1){
      instance(headGenerator)
    } else {
      instance(Gen.oneOf(tGenProvider.value.gen.map(Inr(_)),headGenerator))
    }
  }

}

trait LowPriorityGenProviderInstances {

  import AutoGen.{GenProvider, instance}

  implicit def providerHCons2[K <: Symbol, H, T <: HList]
  (implicit
   hGenProvider: Lazy[GenProvider[H]],
   tGenProvider: Lazy[GenProvider[T]]
  ): GenProvider[FieldType[K,H] :: T] = instance(
    hGenProvider.value.gen.flatMap(f ⇒
      tGenProvider.value.gen.map{ t ⇒
        field[K](f) :: t
      }
    )
  )

  implicit def providerCCons2[K <: Symbol, H, T <: Coproduct, L <: Nat]
  (implicit
   hGenProvider: Lazy[GenProvider[H]],
   tGenProvider: Lazy[GenProvider[T]],
   l: shapeless.ops.coproduct.Length.Aux[H :+: T, L],
   i: ToInt[L]
  ): GenProvider[FieldType[K,H] :+: T] = {
    val headGenerator = hGenProvider.value.gen.map(h ⇒ Inl(field[K](h)))

    if(i() == 1){
      instance(headGenerator)
    } else {
      instance(Gen.oneOf(tGenProvider.value.gen.map(Inr(_)), headGenerator))
    }
  }
}

