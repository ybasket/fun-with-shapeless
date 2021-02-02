package mobimeo.fun.shapeless

import shapeless._
import java.time._

// Taken with small adjustments from https://gist.github.com/calvinlfer/cb38523e4536ecda1b6cf14ee3b03ff5

sealed trait Diff[+A] { self =>
  def getOrElse[A0 >: A](default: A0): A0 = self match {
    case Diff.Identical => default
    case Diff.Change(value) => value
  }
}

object Diff {

  case object Identical extends Diff[Nothing]

  case class Change[A](value: A) extends Diff[A]

  def change[A](value: A): Diff[A] = Change(value)
}

trait Delta[In] {
  type Out

  def apply(existing: In, incoming: In): Out
}

object Delta extends DeltaLowPriority {
  type Aux[I, O] = Delta[I] {
    type Out = O
  }

  def apply[A](implicit proof: Delta[A]): Delta[A] = proof

  private def instance[A](same: (A, A) => Boolean): Delta.Aux[A, Diff[A]] = new Delta[A] {
    type Out = Diff[A]

    override def apply(existing: A, incoming: A): Diff[A] =
      if (same(existing, incoming)) Diff.Identical
      else Diff.Change(incoming)
  }

  implicit val booleanDelta: Aux[Boolean, Diff[Boolean]] = instance[Boolean](_ == _)
  implicit val stringDelta: Aux[String, Diff[String]] = instance[String](_ == _)
  implicit val intDelta: Aux[Int, Diff[Int]] = instance[Int](_ == _)
  implicit val bigIntDelta: Aux[BigInt, Diff[BigInt]] = instance[BigInt](_ == _)
  implicit val localDateDelta: Aux[LocalDate, Diff[LocalDate]] = instance[LocalDate](_ isEqual _)
  implicit val localDateTimeDelta: Aux[LocalDateTime, Diff[LocalDateTime]] = instance[LocalDateTime](_ isEqual _)

  implicit def listDelta[A](implicit
                            diffProof: Aux[A, Diff[A]],
                            orderProof: Ordering[A]
                           ): Aux[List[A], Diff[List[A]]] =
    instance[List[A]] { (incoming, existing) =>
      val iS = incoming.sorted
      val eS = existing.sorted
      iS == eS
    }
}

// Implicit prioritization tricks are used to allow the user to override datatypes
trait DeltaLowPriority {
  // We keep these derivations in low priority so if you provide more specific proof, it will prefer that over these
  implicit val hnilDelta: Delta.Aux[HNil, HNil] = new Delta[HNil] {
    // This must be HNil or the HList will not terminate and the code will not compile
    type Out = HNil

    override def apply(existing: HNil, incoming: HNil): HNil = existing
  }

  implicit def optionDeltaHList[A <: HList, O](implicit
                                            deltaA: Delta.Aux[A, O]
                                           ): Delta.Aux[Option[A], Option[O]] =
    new Delta[Option[A]] {
      override type Out = Option[O]

      override def apply(existing: Option[A], incoming: Option[A]): Out = (existing, incoming) match {
        case (Some(e), Some(i)) => Option(deltaA.apply(e, i))
        case (_, _) => None
      }
    }

  implicit def optionDelta[A](implicit proofNotHList: A =:!= HList): Delta.Aux[Option[A], Diff[Option[A]]] =
    new Delta[Option[A]] {
      type Out = Diff[Option[A]]

      override def apply(existing: Option[A], incoming: Option[A]): Diff[Option[A]] =
        if (existing != incoming) Diff.Change(incoming)
        else Diff.Identical
    }

  implicit def hconsDelta[H, T <: HList, HO, TO <: HList](implicit
                                         proofHead: Delta.Aux[H, HO],
                                         proofTail: Delta.Aux[T, TO]
                                        ): Delta.Aux[H :: T, HO :: TO] = new Delta[H :: T] {
    override type Out = HO :: TO

    override def apply(existing: H :: T, incoming: H :: T): HO :: TO = {
      val diffHead = proofHead(existing.head, incoming.head)
      val diffTail = proofTail(existing.tail, incoming.tail)
      diffHead :: diffTail
    }
  }

  implicit def genericDelta[A, Repr, O](implicit
                                          gen: Generic.Aux[A, Repr],
                                          delta: Delta.Aux[Repr, O]
                                         ): Delta.Aux[A, O] =
    new Delta[A] {
      override type Out = O

      override def apply(existing: A, incoming: A): Out =
        delta(gen.to(existing), gen.to(incoming))
    }
}
