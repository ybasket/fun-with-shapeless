package mobimeo.fun.shapeless

import shapeless.{labelled, _}
import shapeless.labelled.FieldType

import java.time.Instant

object intro extends App {

  // HLists

  val someStuff = 1 :: "2" :: 3.0 :: HNil
  println(someStuff)

  val moreStuff = () :: someStuff
  println(moreStuff)

  val lessStuff = someStuff.tail
  println(lessStuff)

  // Generic

  case class MaasPlatform(name: String, availableSince: Instant, features: List[String])
  val myMaas = MaasPlatform("ScalaMobility", Instant.now(), List("FP", "Trains", "Bikes"))
  println(myMaas)

  val myMaasHlist = Generic[MaasPlatform].to(myMaas)
  println(myMaasHlist)

  val mySavedMaas = Generic[MaasPlatform].from(myMaasHlist)
  println(mySavedMaas)

  Generic[(Int, String, Boolean)]
  println(Generic[Option[String]].to(Some("Mobimeo")))

  println(LabelledGeneric[MaasPlatform].to(myMaas))

}
