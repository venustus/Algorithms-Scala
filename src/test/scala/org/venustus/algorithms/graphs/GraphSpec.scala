package org.venustus.algorithms.graphs

import org.scalatest.{Matchers, FlatSpec}

/**
 * Created by venkat on 13/07/14.
 */
class GraphSpec extends FlatSpec with Matchers {
    val simpleGraph = UndirectedGraph[Int](List(((1, 2), 1), ((2, 3), 2), ((3, 4), 3), ((4, 1), 4), ((1, 3), 5)))

    "Simple connected graph" should "be identified as connected" in {
        simpleGraph.isConnected should be (true)
    }

    it should "return vertices in correct order for DFS" in {
        (simpleGraph depthFirstIterate (1)) should be (List(1, 3, 4, 2))
    }

    it should "return correct minimum spanning tree" in {
        (simpleGraph getMinimumSpanningTree(1)).toSet should be (Set(((1, 2), 1), ((2, 3), 2), ((3, 4), 3)))

        val lines = scala.io.Source.fromURL("file:///Users/venkat/Documents/Coursera/Algorithms-II/src/test/resources/org/venustus/algorithms/graphs/edges.txt").getLines.toList.tail
        val graph =
            UndirectedGraph[Int](lines map ((s: String) => {
                val edgeElems: Array[String] = s.split(' ')
                Pair(Pair(edgeElems(0).toInt, edgeElems(1).toInt), edgeElems(2).toInt)
            }))
        println("Graph has " + graph.numVertices + " vertices and " + graph.numEdges + " edges")
        println("Cost of minimum spanning tree: " + graph.getMinimumSpanningTree(1).foldLeft(0)((acc: Int, edge: Graph.Edge[Int]) => acc + edge._2))
    }
}
