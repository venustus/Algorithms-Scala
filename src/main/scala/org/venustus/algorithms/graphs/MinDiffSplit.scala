package org.venustus.algorithms.graphs

/**
 * Created by venkat on 11/06/15.
 */
object MinDiffSplit {

    /**
     * Split a given undirected graph into two sub-graphs such that sum of weights of nodes
     * in the two sub-graphs differ by minimum amount. Each vertex is assigned a weight
     * which is passed in <code>nodeWeights</code> map.
     *
     * Algorithm:
     * A brute force algorithm would consider each edge and compute the difference of weights obtained
     * by traversing the graph in full. This shall take O(n ** 2) time.
     *
     * However, we can observe that if we just compute tree sums (sum of all the nodes reachable from
     * the current node - while not visiting already visited vertices) then we can do this in O(n) time
     * because we shall have the tree sum of entire graph at the end and we can compute the other parts by
     * computing the differences.
     *
     * Time complexity: O(n)
     *
     * @param t
     * @param nodeWeights
     * @return
     */
    def findMinDiffSplit(t: UndirectedGraph[Int], nodeWeights: Map[Int, Int]) = {
        val visited = (scala.collection.mutable.Map[Int, Boolean]() /: nodeWeights.keys)((acc, k) => {acc.put(k, false); acc})

        def computeTreeSum(n: Int): Pair[Int, Vector[Int]] = {
            val (treeSumOfChildren, ltsc) = ((0, Vector[Int]()) /: (t.nodes getOrElse (n, List())))((acc, edge) => {
                edge match {
                    case (neighbor, _) => {
                        if(!visited(neighbor)) {
                            visited(neighbor) = true
                            val (ts, lts) = computeTreeSum(neighbor)
                            // return a value comprising of aggregate treesum and also the treesum of
                            // child itself appended to a list

                            (acc._1 + ts, acc._2 ++ lts)
                        }
                        else acc
                    }
                }
            })
            (treeSumOfChildren + nodeWeights(n), (treeSumOfChildren + nodeWeights(n)) +: ltsc)
        }
        visited(1) = true
        val (fullTreeSum, treesums) = computeTreeSum(1)
        val minDiffTreeSum = (treesums.minBy((ts) => math.abs((fullTreeSum - ts) - ts)))
        math.abs((fullTreeSum - minDiffTreeSum) - minDiffTreeSum)
    }

}
