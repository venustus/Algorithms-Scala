package org.venustus.algorithms.trees

/**
  * Created by venkat on 17/09/16.
  */


trait BinaryNode[T]

case class EmptyNode[T]() extends BinaryNode[T]

case class TreeNode[T](data: T, children: List[TreeNode[T]])

case class BinaryTreeNode[T](data: T, left: BinaryNode[T], right: BinaryNode[T]) extends BinaryNode[T]

case class Tree[T](root: TreeNode[T])

case class BinaryTree[T](root: BinaryNode[T])

class BinarySearchTree[T <% Ordered[T]](override val root: BinaryNode[T])  extends BinaryTree[T](root)


