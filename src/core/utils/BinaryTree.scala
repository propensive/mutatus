package mutatus.utils

sealed trait BinaryTree[+A] {
  import scala.annotation.tailrec
  import BinaryTree._
  def value: Option[A] = this match {
    case n: Node[A] => Some(n.v)
    case l: Leaf[A] => Some(l.v)
    case Empty      => None
  }

  def left: Option[BinaryTree[A]] = this match {
    case n: Node[A] => Some(n.l)
    case l: Leaf[A] => None
    case Empty      => None
  }

  def right: Option[BinaryTree[A]] = this match {
    case n: Node[A] => Some(n.r)
    case l: Leaf[A] => None
    case Empty      => None
  }
}

object BinaryTree {
  case class Node[A](v: A, l: BinaryTree[A], r: BinaryTree[A]) extends BinaryTree[A]
  case class Leaf[A](v: A) extends BinaryTree[A]
  case object Empty extends BinaryTree[Nothing]
}
