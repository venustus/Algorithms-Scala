package org.venustus.algorithms.graphs

import org.scalatest.{FlatSpec, Matchers}

/**
 * Created by venkat on 11/06/15.
 */
class MinDiffSplitSpec extends FlatSpec with Matchers {

    "Minimum diff split algorithm" should "split the trees correctly" in {
        (MinDiffSplit findMinDiffSplit (new UndirectedGraph[Int](List(((2, 1), 0), ((3, 2), 0), ((5, 2), 0), ((4, 5), 0), ((6, 5), 0))),
            Map[Int, Int](1 -> 100, 2 -> 200, 3 -> 100, 4 -> 500, 5 -> 100, 6 -> 600))) should be (400)
    }

    "Minimum diff split algorithm" should "split the graph correctly" in {
        val src = scala.io.Source.fromURL("file:///Users/venkat/Documents/Projects/Algorithms/src/test/resources/org/venustus/algorithms/search/min-diff-split-test3.txt").getLines.toList
        val numVertices = src.head.toInt
        val nodeWeightsArr = src.tail.head.split(" ").map(_.toInt)
        val nodeWeights = (Map[Int, Int]() /: (1 to numVertices))((acc, i) => { acc updated (i, nodeWeightsArr(i - 1)) })
        val edges = (List[Graph.Edge[Int]]() /: src.tail.tail)((edges, line) => {
            val Array(vertex1, vertex2) = line.split(" ").map(_.toInt)
            ((vertex2, vertex1), 0) :: edges
        })
        MinDiffSplit findMinDiffSplit (new UndirectedGraph[Int](edges), nodeWeights) should be (504)
    }

}
