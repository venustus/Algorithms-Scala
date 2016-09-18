package org.venustus.algorithms.trees

/**
  * Created by venkat on 17/09/16.
  */
object Trees {

    /**
      * Finds and returns least common ancestor of two given elements that are assumed to be present
      * in a given binary search tree.
      *
      * Time complexity: O(log n) where n is number of nodes in the tree and assuming binary search
      * tree is mostly balanced
      * @param tree
      * @param a
      * @param b
      * @tparam T
      * @return
      * @throws IllegalArgumentException if all the elements are greater than or less than both of the given elements
      */
    def findLeastCommonAncestor[T <% Ordered[T]](tree: BinarySearchTree[T], a: T, b: T): BinaryNode[T] = {
        def lcaHelper(root: BinaryNode[T]): BinaryNode[T] = {
            root match {
                case EmptyNode() => throw new IllegalArgumentException("Invalid input")
                case BinaryTreeNode(data, left, right) =>
                    if(data >= a && data <= b) root
                    else {
                        if(data < a) lcaHelper(right)
                        else lcaHelper(left)
                    }
            }
        }
        if(a > b) findLeastCommonAncestor(tree, b, a)
        else lcaHelper(tree.root)
    }

    /**
      * Finds least common ancestor of two given elements in a general binary tree
      * (not necessarily a binary search tree).
      *
      * Time complexity: O(n) where n is number of nodes in the tree.
      * @param tree
      * @param a
      * @param b
      * @tparam T
      * @return
      */
    def findLeastCommonAncestor[T](tree: BinaryTree[T], a: T, b: T): Option[BinaryNode[T]] = {
        def lcaHelper(root: BinaryNode[T]): (Option[BinaryNode[T]], Option[BinaryNode[T]], Option[BinaryNode[T]]) = {
            root match {
                case EmptyNode() => (None, None, None)
                case BinaryTreeNode(data, left, right) =>
                    val (aFoundLeft, bFoundLeft, ancestorFoundLeft) = lcaHelper(left)
                    if(ancestorFoundLeft.isDefined) (aFoundLeft, bFoundLeft, ancestorFoundLeft)
                    else {
                        val (aFoundRight, bFoundRight, ancestorFoundRight) = lcaHelper(right)
                        if(ancestorFoundRight.isDefined) (aFoundRight, bFoundRight, ancestorFoundRight)
                        else {
                            val aFound = if(aFoundLeft.isDefined) aFoundLeft
                                         else if(aFoundRight.isDefined) aFoundRight
                                         else if(data == a) Some(root) else None
                            val bFound = if(bFoundLeft.isDefined) bFoundLeft
                                         else if(bFoundRight.isDefined) bFoundRight
                                         else if(data == b) Some(root) else None
                            val ancFound = if(aFound.isDefined && bFound.isDefined) Some(root) else None
                            (aFound, bFound, ancFound)
                        }
                    }
            }
        }
        val (_, _, ancFound) = lcaHelper(tree.root)
        ancFound
    }

}
