package org.andrewconner.spot.core

import play.api.data.validation.ValidationError
import play.api.libs.json._

package object json {
  object ReadIfPossible {
    def emptyReads[T] = Reads { j => JsSuccess(Option.empty[T]) }
    implicit class PimpedJsPath(jsp: JsPath) {
      // validation with readIfPossible ALWAYS succeeds, be careful when using it
      def readIfPossible[T](implicit reads: Reads[T]): Reads[Option[T]] = {
        jsp.readNullable[T] orElse emptyReads
      }
      def formatIfPossible[T](implicit reads: Reads[T], writes: Writes[T]): OFormat[Option[T]] = {
        OFormat(readIfPossible, jsp.writeNullable)
      }
    }
  }

  object MapFormat {
    def mapReads[K, V](fromKey: String => Option[K])(implicit vReads: Reads[V]): Reads[Map[K, V]] = Reads { j =>
      j.validate[JsObject].flatMap { jsObj =>
        val pairs = jsObj.fields.map {
          case (jsK, jsV) => for { k <- fromKey(jsK); v <- vReads.reads(jsV).asOpt } yield k -> v
        }
        pairs.zipWithIndex.find(_._1.isEmpty) match {
          case Some((_, failedIdx)) => JsError(s"Could not parse ${jsObj.fields(failedIdx)} as a key-value pair")
          case None => JsSuccess(pairs.map(_.get).toMap)
        }
      }
    }

    def mapWrites[K, V](toKey: K => String)(implicit vWrites: Writes[V]): Writes[Map[K, V]] = Writes { o =>
      JsObject(o.toSeq.map { case (k, v) => toKey(k) -> vWrites.writes(v) })
    }
    def mapFormat[K, V](toKey: K => String, fromKey: String => Option[K])(implicit vFormat: Format[V]): Format[Map[K, V]] =
      Format(mapReads(fromKey), mapWrites(toKey))
  }

  object TupleFormat {
    /* Inspired from https://coderwall.com/p/orci8g */
    implicit def tuple2Reads[A, B](implicit aReads: Reads[A], bReads: Reads[B]): Reads[(A, B)] = Reads[(A, B)] {
      case JsArray(arr) if arr.size == 2 => for {
        a <- aReads.reads(arr(0))
        b <- bReads.reads(arr(1))
      } yield (a, b)
      case _ => JsError(Seq(JsPath() -> Seq(ValidationError("Expected array of two elements"))))
    }

    implicit def tuple2Writes[A, B](implicit aWrites: Writes[A], bWrites: Writes[B]): Writes[(A, B)] = Writes[(A, B)] {
      tup: (A, B) => JsArray(Seq(aWrites.writes(tup._1), bWrites.writes(tup._2)))
    }

    implicit def tuple2Format[A, B](implicit aFormat: Format[A], bFormat: Format[B]): Format[(A, B)] = {
      Format(tuple2Reads[A, B], tuple2Writes[A, B])
    }

    implicit def tuple3Reads[A, B, C](implicit aReads: Reads[A], bReads: Reads[B], cReads: Reads[C]): Reads[(A, B, C)] = Reads[(A, B, C)] {
      case JsArray(arr) if arr.size == 3 => for {
        a <- aReads.reads(arr(0))
        b <- bReads.reads(arr(1))
        c <- cReads.reads(arr(2))
      } yield (a, b, c)
      case _ => JsError(Seq(JsPath() -> Seq(ValidationError("Expected array of three elements"))))
    }

    implicit def tuple3Writes[A, B, C](implicit aWrites: Writes[A], bWrites: Writes[B], cWrites: Writes[C]): Writes[(A, B, C)] = new Writes[(A, B, C)] {
      def writes(tuple: (A, B, C)) = JsArray(Seq(aWrites.writes(tuple._1), bWrites.writes(tuple._2), cWrites.writes(tuple._3)))
    }

    implicit def tuple3Format[A, B, C](implicit aFormat: Format[A], bFormat: Format[B], cFormat: Format[C]): Format[(A, B, C)] = {
      Format(tuple3Reads[A, B, C], tuple3Writes[A, B, C])
    }
  }
}