/*
   ╔═══════════════════════════════════════════════════════════════════════════════════════════════════════════╗
   ║ Quarantine, version 0.1.0. Copyright 2019-2020 Jon Pretty, Propensive OÜ.                                 ║
   ║                                                                                                           ║
   ║ The primary distribution site is: https://propensive.com/                                                 ║
   ║                                                                                                           ║
   ║ Licensed under  the Apache License,  Version 2.0 (the  "License"); you  may not use  this file  except in ║
   ║ compliance with the License. You may obtain a copy of the License at                                      ║
   ║                                                                                                           ║
   ║     http://www.apache.org/licenses/LICENSE-2.0                                                            ║
   ║                                                                                                           ║
   ║ Unless required  by applicable law  or agreed to in  writing, software  distributed under the  License is ║
   ║ distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. ║
   ║ See the License for the specific language governing permissions and limitations under the License.        ║
   ╚═══════════════════════════════════════════════════════════════════════════════════════════════════════════╝
*/

package quarantine

import scala.reflect._
import scala.util._, control.NonFatal

import language.existentials
import language.higherKinds
import language.implicitConversions

object `package` {
  implicit def toCatch[T](fn: => T): Catch[T] = new Catch[T](() => fn)
}

class Catch[T](val fn: () => T) extends AnyVal {
  def check[T1 >: T](pf: PartialFunction[Throwable, T1]): T1 = try fn() catch pf
}

abstract class Domain[ExcType <: Exception: ClassTag] {

  type ExceptionType = ExcType

  object Result {
    def apply[T](fn: => T): Result[T] = try Answer(fn) catch {
      case exception: ExceptionType => Error(exception)
      case NonFatal(exception)        => Surprise(exception)
    }

    def from[F[+_]: From, T](value: F[T]): Result[T] = implicitly[From[F]].convert(value)

    object From {
      implicit val fromOption: From[Option] = new From[Option] {
        def convert[T](value: Option[T]): Result[T] = value match {
          case Some(value) => Answer(value)
          case None        => Surprise(new NoSuchElementException())
        }
      }

      implicit val fromTry: From[Try] = new From[Try] {
        def convert[T](value: Try[T]): Result[T] = value match {
          case Success(value)                    => Answer(value)
          case Failure(exception: ExceptionType) => Error(exception)
          case Failure(exception)                => Surprise(exception)
        }
      }
    }

    trait From[F[+_]] { def convert[T](value: F[T]): Result[T] }

    object To {
      implicit val toOption: To[Option] = new To[Option] {
        def convert[T](value: Result[T]): Option[T] = value match {
          case Answer(value)   => Some(value)
          case Error(error)    => None
          case Surprise(error) => None
        }
      }

      implicit val toTry: To[Try] = new To[Try] {
        def convert[T](value: Result[T]): Try[T] = value match {
          case Answer(value)   => Success(value)
          case Error(error)    => Failure(error)
          case Surprise(error) => Failure(error)
        }
      }
    }

    trait To[F[+_]] { def convert[T](value: Result[T]): F[T] }
  }


  object Unsurprising {
    def unapply[T](value: Result[T]): Option[Unsurprising[T]] = value match {
      case Answer(answer) => Some(Answer(answer))
      case Error(error)   => Some(Error(error))
      case Surprise(error) => None
    }
  }

  sealed trait Unsurprising[+T] {
    def adapt[D <: Domain[_]](implicit domain: D, mitigator: Mitigator[D]): D#Result[T]
    def recover[T1 >: T](fn: ExceptionType => T1): T1
  }
  
  object Erroneous {
    def unapply[T](value: Result[T]): Option[Erroneous] = value match {
      case Answer(answer)  => None
      case Error(error)    => Some(Error(error))
      case Surprise(error) => Some(Surprise(error))
    }
  }

  sealed trait Erroneous { def throwable: Throwable }

  sealed abstract class Result[+T](protected val either: Either[Throwable, T]) { result =>
    def flatMap[U](fn: T => Result[U]): Result[U] = either.flatMap(fn(_).either) match {
      case Left(error: ExceptionType) => Error(error)
      case Left(error)                => Surprise(error)
      case Right(value)               => Answer(value)
    }
    
    def map[U](fn: T => U): Result[U] = flatMap[U](fn.andThen(Result(_)))
    def foreach(fn: T => Unit): Unit = map(fn)
    def to[F[+_]: Result.To]: F[T] = implicitly[Result.To[F]].convert(result)

    def adapt[D <: Domain[_]](implicit domain: D, mitigator: Mitigator[D]): D#Result[T] = result match {
      case Answer(value)       => domain.Answer(value)
      case Error(error)        => mitigator.handle(error)
      case Surprise(exception) => mitigator.anticipate(exception)
    }

    def mitigate[E](fn: ExceptionType => E)(implicit domain: Domain[_ >: E]): domain.Result[T] = result match {
      case Answer(value)       => domain.Answer(value)
      case Error(error)        => domain.Error(fn(error) match { case e: domain.ExceptionType => e })
      case Surprise(exception) => domain.Surprise(exception)
    }

    def prescribe[E](error: E)(implicit domain: Domain[_ >: E]): domain.Result[T] = result match {
      case Answer(value)       => domain.Answer(value)
      case Error(_)            => domain.Error(error)
      case Surprise(exception) => domain.Surprise(exception)
    }

    def mollify[E](error: E)(implicit domain: Domain[_ >: E]): domain.Result[T] = result match {
      case Answer(value) => domain.Answer(value)
      case Error(_)      => domain.Error(error)
      case Surprise(_)   => domain.Error(error)
    }

    def recover[S >: T](fn: ExceptionType => S): S = result match {
      case Answer(value)   => value
      case Error(error)    => fn(error)
      case Surprise(error) => throw error
    }

    def extenuate(fn: Throwable => ExceptionType): Result[T] = result match {
      case Answer(value)   => Answer(value)
      case Error(error)    => Error(error)
      case Surprise(error) => Error(fn(error))
    }

    def apprehend[E](fn: PartialFunction[ExceptionType, E])(implicit domain: Domain[_ >: E]): domain.Result[T] =
      result match {
        case Answer(value)   => domain.Answer(value)
        case Error(error)    => if(fn.isDefinedAt(error)) domain.Error(fn(error)) else domain.Surprise(error)
        case Surprise(error) => domain.Surprise(error)
      }

    def pacify[T1 >: T](default: T1): T1 = result match {
      case Answer(value) => value
      case _             => default
    }

    def assume(implicit domain: Domain[_]): domain.Result[T] = result match {
      case Answer(value)   => domain.Answer(value)
      case Error(error)    => domain.Surprise(error)
      case Surprise(error) => domain.Surprise(error)
    }
  }

  case class Answer[+T](value: T) extends Result[T](Right(value)) with Unsurprising[T]
  
  case class Error(error: ExceptionType) extends Result[Nothing](Left(error)) with Unsurprising[Nothing] with
      Erroneous {
    def throwable: Throwable = error
  }
  
  case class Surprise(error: Throwable) extends Result[Nothing](Left(error)) with Erroneous {
    def throwable: Throwable = error
  }

  trait Mitigator[D <: Domain[_]] {
    def handle[T](exception: ExceptionType): D#Result[T]
    def anticipate[T](throwable: Throwable): D#Result[T]
  }

  def mitigate[S <: Exception](domain: Domain[S])
                              (handler: ExceptionType => domain.ExceptionType)
                              : Mitigator[domain.type] = new Mitigator[domain.type] {
    def anticipate[T](throwable: Throwable): domain.Surprise = domain.Surprise(throwable)
    def handle[T](exception: ExceptionType): domain.Result[T] = domain.Error(handler(exception))
  }
}
