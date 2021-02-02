package mobimeo.fun.shapeless

case class BackendDeveloper(name: String, monitors: Int, stsIncidentsHandled: BigInt)

object DiffApp extends App {

  val astrid = BackendDeveloper("Astrid", 2, BigInt("194585473875257385235347"))
  val viktor = BackendDeveloper("Viktor", 2, BigInt("234892348923489238423984"))

  println(Delta[BackendDeveloper].apply(astrid, viktor))

}
