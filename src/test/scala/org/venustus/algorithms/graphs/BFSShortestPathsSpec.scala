package org.venustus.algorithms.graphs

import org.scalatest.{FlatSpec, Matchers}

/**
 * Created by venkat on 28/06/15.
 */
class BFSShortestPathsSpec extends FlatSpec with Matchers {
    "Shortest paths algorithm in a unit weight graph" should "return correct set of shortest path distances" in {
        val g = (new UndirectedGraph[Int](List(((1, 2), 6), ((1, 3), 6)))) addNode (4)
        BFSShortestPaths findShortestPaths(g, 1) should be (Map[Int, Int](2 -> 6, 3 -> 6, 4 -> -1))
    }
}
