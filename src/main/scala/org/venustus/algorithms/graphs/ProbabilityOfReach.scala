package org.venustus.algorithms.graphs

import scala.collection.mutable

/**
 * Created by venkat on 27/06/15.
 */
object ProbabilityOfReach {

    /**
     * Finds probability of reaching <code>destination</code> in a directed graph
     * when probability of starting at each of the other nodes is given.
     *
     * Basic approach is depth first search, we have to be clever to avoid repeating
     * depth first search on same vertices again and again.
     *
     * A brute force way would almost take O(n ** 2) time - it performs depth first search
     * starting at each vertex and verifies if the destination is reachable.
     *
     * An important observation is that when you are performing depth first search for one
     * vertex, you can update reachability for a whole lot of other vertices in the
     * same connected component. In particular, you can update reachability for vertices
     * which have been finished in that run - their loop over their neighbors is complete.
     * Once this loop is complete, we know the reachability from that vertex and we never need
     * to explore this vertex again, if we cache this result.
     *
     * Time complexity: O(n) where n is number of vertices in the graph.
     *
     * @param g
     * @param destination
     * @param startProbabilities
     */
    def findProbabilityOfReach(g: DirectedGraph[Int], destination: Int, startProbabilities: Map[Int, Double]) = {
        val reachableMap = (mutable.Map[Int, Boolean]() /: (g.nodes.keys))((acc, n) => acc updated (n, false))
        def search(startNode: Int, visitedMap: mutable.Map[Int, Boolean]): Boolean = {
            if(startNode == destination) true
            else {
                val finalAns = (false /: g.nodes(startNode))((acc, edge) => {
                    edge match {
                        case (neighbor, _) => {
                            if(visitedMap getOrElse (neighbor, false)) acc
                            else {
                                visitedMap(neighbor) = true
                                acc || (reachableMap(neighbor) || search(neighbor, visitedMap))
                            }
                        }
                    }
                })
                reachableMap(startNode) = finalAns
                finalAns
            }
        }
        (0D /: (1 to g.nodes.size - 1))((acc, n) => {
            if(reachableMap(n) || search(n, mutable.Map[Int, Boolean](n -> true))) {
                acc + startProbabilities(n)
            }
            else {
                acc
            }

        })
    }

    def main(args: Array[String]) {
        val numTestCases = readInt
        for(_ <- 1 to numTestCases) {
            val numNodes = readInt + 1
            val edges = (List[Graph.Edge[Int]]() /: (1 to numNodes))((acc, i) => {
                val edgesFromNode = readLine.split(" ").map(_.toInt)
                ((edgesFromNode drop (1)) map (d => Pair(Pair[Int, Int](i, d), 0))).toList ::: acc
            })
            val probabilities = (Map[Int, Double]() /: readLine.split(" ").map(_.toDouble).zipWithIndex)((acc, ith) => acc updated (ith._2 + 1, ith._1))
            println(findProbabilityOfReach(new DirectedGraph[Int](edges), numNodes, probabilities))
        }
    }

}
