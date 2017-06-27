package hmrc.smartstub

import shapeless._
import shapeless.labelled._
import org.scalacheck._
import hmrc.smartstub._

trait GenProvider[A] {
  def gen: Gen[A] = genN("")
  def genN(s: String): Gen[A] = gen
}

object AutoGen {

  // summoner
  def apply[A](implicit e: GenProvider[A]): Gen[A] = {
    e.gen
  }

  // instance constructors
  def instanceF[A](z: Gen[A]): GenProvider[A] = new GenProvider[A] {
    override val gen = z
  }

  def instance[A](v: String => Gen[A]): GenProvider[A] = new GenProvider[A] {
    override def genN(s:String) = v(s)
  }

  // 'basic' types
  implicit val providerInt = instance(_ match {
    case "age" => Gen.choose(1,80)
    case _ => Gen.choose(1,1000)
  })

  implicit val providerString = instance(
    _ match {
      case "forename" => Gen.forename
      case "surname" => Gen.surname
      case "gender" => Gen.oneOf("male", "female")
      case "nino" => Enumerable.instances.ninoEnum.gen
      case "utr" => Enumerable.instances.utrEnum.gen
      case "sector" => Gen.sector
      case _ => Gen.alphaStr
    }
  )
  implicit val providerBoolean = instanceF(Gen.oneOf(true,false))

  // Collection types
  implicit def providerOpt[A](implicit inner: GenProvider[A]): GenProvider[Option[A]]  =
    instance { x => 
      Gen.option(inner.genN(x))
    }

  implicit def providerList[A](implicit inner: GenProvider[A]): GenProvider[List[A]]  =
    instance { x => 
      Gen.listOf(inner.genN(x))
    }

  implicit def providerSeq[A](implicit inner: GenProvider[A]): GenProvider[Seq[A]]  =
    instance { x => 
      Gen.listOf(inner.genN(x))
    }

  // HLists
  implicit val providerHNil: GenProvider[HNil] = instanceF(Gen.const(HNil))

  implicit def providerHListLabelled[S <: Symbol, H, T <: HList](
    implicit
      witness: Witness.Aux[S],
    providerHead: Lazy[GenProvider[H]],
    providerTail: GenProvider[T]
  ): GenProvider[FieldType[S,H] :: T] = {
    val fieldname = witness.value.name
    instanceF {
      for {
        h <- providerHead.value.genN(fieldname)
        t <- providerTail.gen
      } yield {
        field[S](h) :: t
      }
    }
  }

  implicit def providerHList[H, T <: HList](
    implicit
    providerHead: Lazy[GenProvider[H]],
    providerTail: GenProvider[T]
  ): GenProvider[H :: T] = 
    instanceF {
      for {
        h <- providerHead.value.gen
        t <- providerTail.gen
      } yield {
        h :: t
      }
    }


  // generic
  implicit def zeroGeneric[A, R <: HList](
    implicit gen: LabelledGeneric.Aux[A, R],
    enc: Lazy[GenProvider[R]]
  ): GenProvider[A] = instanceF{enc.value.gen.map{gen.from}}
}
