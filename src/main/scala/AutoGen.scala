package uk.gov.hmrc.smartstub

import shapeless._
import shapeless.labelled._
import org.scalacheck._
import org.scalacheck.rng.Seed
import shapeless.ops.nat.ToInt

object AutoGen extends LowPriorityGenProviderInstances {

  trait GenProvider[A] {
    val gen: Gen[A]
  }

  case class State private (genderSeed: Option[Seed])

  object State {
    def apply(seed: Long): State = State(Some(Seed(seed)))
  }

  def apply[A](implicit e: GenProvider[A]): Gen[A] = e.gen

  // instance constructor
  def instance[A](f: Gen[A]): GenProvider[A] = new GenProvider[A] {
    override val gen: Gen[A] = f
  }

  // Named types
  implicit def providerSeqNamed[A](fieldName: String, s: State)(implicit inner: String ⇒ GenProvider[A]): GenProvider[Seq[A]] =
    instance(Gen.listOf(inner(fieldName).gen))

  implicit def providerSetNamed[A](fieldName: String, s: State)(implicit inner: String ⇒ GenProvider[A]): GenProvider[Set[A]] =
    instance(Gen.listOf(inner(fieldName).gen).map(_.toSet))

  implicit def providerVectorNamed[A](fieldName: String, s: State)(implicit inner: String ⇒ GenProvider[A]): GenProvider[Vector[A]] =
    instance(Gen.listOf(inner(fieldName).gen).map(l ⇒l.toVector))

  implicit def providerOptionNamed[A](fieldName: String, s: State)(implicit inner: String ⇒ GenProvider[A]): GenProvider[Option[A]] =
    instance(Gen.option(inner(fieldName).gen))

  implicit def providerIntNamed(fieldName: String, s: State): GenProvider[Int] = instance ({
    fieldName.toLowerCase match {
      case "age" => Gen.choose(1, 80)
      case _ => Gen.choose(1, 1000)
    }
  })

  implicit def providerStringNamed(fieldName: String, s: State): (State, GenProvider[String]) = instance ({
    fieldName.toLowerCase match {
      case "forename" | "firstname" ⇒
        val genderSeed = s.genderSeed.getOrElse(Seed.random())
        val genderGen = Gen.gender.withPerturb(_ ⇒ genderSeed)
        val nameGenerator = genderGen.flatMap(Gen.forename)
        s.copy(genderSeed = Some(genderSeed)) → instance(nameGenerator)

      case "surname" | "lastname" | "familyname" ⇒ s → instance(Gen.surname)
      case x if x.toLowerCase.contains("address") ⇒ s → Gen.ukAddress.map{_.mkString(", ")}
      case "gender" | "sex" => s → Gen.oneOf("male", "female")
      case "nino" ⇒ s → Enumerable.instances.ninoEnum.gen
      case "utr" ⇒ s → Enumerable.instances.utrEnum.gen
      case _ ⇒ s → Gen.alphaStr
    }
  })

  implicit def providerBooleanNamed(fieldName: String): GenProvider[Boolean] =
    instance(Gen.oneOf(true,false))

   implicit def providerUnnamed[A](implicit g: String ⇒ GenProvider[A]): GenProvider[A] = g("")


  // generic instance

  implicit def providerGeneric[A, H, T]
  (implicit
   generic: LabelledGeneric.Aux[A,T],
   hGenProvider: Lazy[State ⇒ GenProvider[T]]
  ): State ⇒ GenProvider[A] = (s: State) ⇒
    instance(hGenProvider.value(s).gen.map(generic.from))

  // HList instances

  implicit def providerHNil(s: State): GenProvider[HNil] = instance(Gen.const(HNil))

  implicit def providerHCons[K <: Symbol, H, T <: HList](s: State)(
    implicit
   witness: Witness.Aux[K],
   hGenProvider: Lazy[(String, State) ⇒ (State, GenProvider[H])],
   tGenProvider: Lazy[State ⇒ GenProvider[T]]
  ): GenProvider[FieldType[K,H] :: T] =  {
    val (nextState, gen) = hGenProvider.value(witness.value.name, s)
    
    instance(gen.gen.flatMap { f =>
      tGenProvider.value(nextState).gen.map { t ⇒
        field[K](f) :: t
      }
    })
  }

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

