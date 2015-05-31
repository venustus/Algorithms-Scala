package org.venustus.algorithms.graphs

/**
 * Created by venkat on 30/05/15.
 */
object EvenTree {

    /**
     * Finds maximum number of edges that can be removed from a tree such that all the trees
     * in the resulting forest have even number of nodes.
     *
     * Algorithm:
     *
     * At any given node, recursively compute the maximum number of nodes to remove from each of the child
     * sub trees. In the recursive call, also compute the total number of children. If any of the child sub tree
     * has an even number of children, then edge to that child can be removed.
     *
     * Time complexity: O(n) where n is the size of the tree
     *
     * @param t
     * @return
     */
    def findMaxEdgesToRemoveForEvenForest(t: Tree[Int]) = {
        def findMaxEdgesHelper(n: Int): (Int, Int) = {
            if(!(t.nodesReverse contains (n))) (0, 1)
            else {
                val ans = ((0, 0) /: t.nodesReverse(n))((acc, neighbor) => {
                    acc match {
                        case (maxEdgesToRemove, childCount) => {
                            val (r1, r2) = findMaxEdgesHelper(neighbor._1)
                            if (r2 % 2 == 0) (maxEdgesToRemove + r1 + 1, childCount + r2)
                            else (maxEdgesToRemove + r1, childCount + r2)
                        }
                    }
                })
                (ans._1, ans._2 + 1)
            }
        }
        findMaxEdgesHelper(t.root)._1
    }

}
