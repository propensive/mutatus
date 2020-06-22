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

import language.higherKinds

import scala.concurrent._
import scala.util._, control._
import scala.annotation.unchecked.{uncheckedVariance => uv}

object boxing {
  implicit object id extends Box {
    type Wrap[+E <: Throwable, +T] = T
    def fail[E <: Throwable](error: => E): Wrap[E, Nothing] = throw error
    def succeed[T](value: => T): Wrap[Nothing, T] = value
  }

  implicit object option extends Box {
    type Wrap[+E <: Throwable, +T] = Option[T]
    def fail[E <: Throwable](error: => E): Wrap[E, Nothing] = None
    def succeed[T](value: => T): Wrap[Nothing, T] = Some(value)
  }
  
  implicit object either extends Box {
    type Wrap[+E <: Throwable, +T] = Either[E, T]
    def fail[E <: Throwable](error: => E): Wrap[E, Nothing] = Left(error)
    def succeed[T](value: => T): Wrap[Nothing, T] = Right(value)
  }
}

object Box {
  implicit object `try` extends Box {
    type Wrap[+E <: Throwable, +T] = Try[T]
    def fail[E <: Throwable](error: => E): Wrap[E, Nothing] = Failure(error)
    def succeed[T](value: => T): Wrap[Nothing, T] = Success(value)
  }

  def apply[T](fn: => T)(implicit box: Box): box.Wrap[Throwable, T] = box.nonFatal[T](fn)

  def fromTry[E <: Throwable, T](`try`: => Try[T])(implicit box: Box): box.Wrap[Throwable, T] =
    if(`try`.isSuccess) box.succeed(`try`.get) else box.fail(`try`.failed.get)

  object From1 {
    implicit object `try` extends From1[Try] {
      def convert[T](boxed: => Try[T], box: Box): box.Wrap[Throwable, T] =
        if(boxed.isSuccess) box.succeed(boxed.get) else box.fail(boxed.failed.get)
    }
   
    implicit object option extends From1[Option] {
      def convert[T](boxed: => Option[T], box: Box): box.Wrap[Throwable, T] =
        if(boxed.isDefined) box.succeed(boxed.get) else box.fail(new NoSuchElementException("None.get"))
    }
  }

  object From2 {
    implicit object either extends From2[Either] {
      def convert[E <: Throwable, T](boxed: => Either[E, T], box: Box): box.Wrap[E, T] =
        if(boxed.isLeft) box.fail(boxed.left.get) else box.succeed(boxed.right.get)
    }
  }

  trait From1[Boxed[_]] {
    def convert[T](boxed: => Boxed[T], box: Box): box.Wrap[Throwable, T]
  }

  trait From2[Boxed[+_ <: Throwable, _]] {
    def convert[E <: Throwable, T](boxed: => Boxed[E, T], box: Box): box.Wrap[E, T]
  }

  def from[F[+_]: From1, T](value: => F[T])(implicit box: Box): box.Wrap[Throwable, T] =
    implicitly[From1[F]].convert(value, box)

  def from[F[+_ <: Throwable, +_]: From2, E <: Throwable, T](value: => F[E, T])(implicit box: Box): box.Wrap[E, T] =
    implicitly[From2[F]].convert(value, box)
}

trait Box {
  type Wrap[+_ <: Throwable, +_]
  
  def fail[E <: Throwable](error: => E): Wrap[E, Nothing]
  def succeed[T](value: => T): Wrap[Nothing, T]
  
  def nonFatal[T](value: => T): Wrap[Throwable, T] = try succeed[T](value) catch { case NonFatal(e) => fail(e) }

  def capture[E <: Throwable: scala.reflect.ClassTag, T](value: => T) =
    try succeed[T](value) catch { case e: E => fail(e) }

  def adapt[E <: Throwable: scala.reflect.ClassTag, E2 <: Throwable, T]
      (value: => T)
      (adaptation: E => E2)
      : Wrap[E2, T] =
    try succeed[T](value) catch { case e: E => fail(adaptation(e)) }
}

