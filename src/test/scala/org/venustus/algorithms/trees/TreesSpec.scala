package org.venustus.algorithms.trees

import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by venkat on 17/09/16.
  */
class TreesSpec extends FlatSpec with Matchers {

    "Sub routine to find least common ancestor" should "find correct LCA" in {
        val myTree = new BinarySearchTree(BinaryTreeNode(20, BinaryTreeNode(8, BinaryTreeNode(4, EmptyNode(), EmptyNode()), BinaryTreeNode(12, BinaryTreeNode(10, EmptyNode(), EmptyNode()), BinaryTreeNode(14, EmptyNode(), EmptyNode()))), BinaryTreeNode(22, EmptyNode(), EmptyNode())))
        Trees findLeastCommonAncestor(myTree, 4, 14) match {
            case BinaryTreeNode(data, _, _) => data should be (8)
            case EmptyNode() => fail("Could not find right LCA")
        }
    }

}
