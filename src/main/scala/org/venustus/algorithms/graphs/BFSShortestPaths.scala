package org.venustus.algorithms.graphs

import scala.collection.mutable

/**
 * Created by venkat on 28/06/15.
 */
object BFSShortestPaths {

    /**
     * Finds shortest paths in an undirected graph where all edges have equal weight,
     * from a given start vertex.
     *
     * We basically follow the breadth-first-search approach using scala queue.
     *
     * Time complexity: O(n) where n is the number of vertices in the graph
     *
     * @param g
     * @param startVertex
     * @return
     */
    def findShortestPaths(g: UndirectedGraph[Int], startVertex: Int) = {
        val bfsQ = mutable.Queue[Int]()
        val visitedMap = mutable.Map[Int, Boolean]()
        val distances = mutable.Map[Int, Int]()
        bfsQ enqueue (startVertex)
        distances(startVertex) = 0
        visitedMap(startVertex) = true
        while(!(bfsQ isEmpty)) {
            val next = bfsQ dequeue()
            for(n <- g.nodes(next)) {
                n match {
                    case (neighbor, weight) => {
                        if (!(visitedMap contains (neighbor))) {
                            bfsQ enqueue (neighbor)
                            visitedMap(neighbor) = true
                            distances(neighbor) = distances(next) + weight
                        }
                    }
                }
            }
        }
        for(n <- g.nodes.keys) {
            if(!(distances contains (n))) distances(n) = -1
        }
        distances remove (startVertex)
        distances
    }

}
