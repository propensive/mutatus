package mutatus.utils

sealed trait BinaryTree[+A] {
  def getValue: Option[A]
  def leftTree: Option[BinaryTree[A]]
  def rightTree: Option[BinaryTree[A]]
}

object BinaryTree {
  case class Node[A](value: A, left: BinaryTree[A], right: BinaryTree[A])
      extends BinaryTree[A] {
    def getValue: Option[A] = Some(value)
    def leftTree: Option[BinaryTree[A]] = Some(left)
    def rightTree: Option[BinaryTree[A]] = Some(right)
  }
  case class Leaf[A](value: A) extends BinaryTree[A] {
    def getValue: Option[A] = Some(value)
    def leftTree: Option[BinaryTree[A]] = None
    def rightTree: Option[BinaryTree[A]] = None
  }
  case object Empty extends BinaryTree[Nothing] {
    def getValue: Option[Nothing] = None
    def leftTree: Option[BinaryTree[Nothing]] = None
    def rightTree: Option[BinaryTree[Nothing]] = None
  }
}
