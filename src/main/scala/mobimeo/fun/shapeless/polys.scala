package mobimeo.fun.shapeless

import shapeless._

object polys extends App {

  object LoveMe extends Poly1 {
    implicit val loveInts = at[Int](i => s"I love you, my dear Int $i")
    implicit val loveBooleans = at[Boolean](b => s"I love you ${b}ly")
    implicit val loveString = at[String](s => s.length)
    implicit def loveAnyVals[A <: AnyVal] = at[A](a => s"I love all primitive values: $a")
  }

  println(LoveMe(1))
  println(LoveMe(true))
  println(LoveMe("Make ..., not war!"))
  println(LoveMe(3.4))
  //println(LoveMe(List(1)))

  /*
    Imagine as
    trait LoveMe[T] {
      type Out
      def apply(t: T): Out
    }

    object LoveMe {
      implicit val loveInts = new LoveMe[Int] {
        type Out = String
        def apply(t: Int): String
      }
      ...
    }

    Or in Prolog:
    loveme(integer(A), B) :- B is string("I love you, my dear Int %s", A).
   */

}
