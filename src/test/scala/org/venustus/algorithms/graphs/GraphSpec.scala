package org.venustus.algorithms.graphs

import org.scalatest.{Matchers, FlatSpec}

/**
 * Created by venkat on 13/07/14.
 */
class GraphSpec extends FlatSpec with Matchers {
    val simpleGraph = new UndirectedGraph[Int](List(((1, 2), 1), ((2, 3), 2), ((3, 4), 3), ((4, 1), 4), ((1, 3), 5)))
    val simpleDirectedGraph = new DirectedGraph[Int](List(((1, 2), 1), ((2, 3), 2), ((3, 4), 3), ((4, 1), 4), ((1, 3), 5)))
    val simpleDisconnectedDirectedGraph = new DirectedGraph[Int](List(((1, 2), 1), ((2, 3), 2), ((3, 4), 3), ((4, 1), 4), ((1, 3), 5),
        ((5, 2), 5), ((5, 3), 5)))
    val simpleDag = new DirectedGraph[Int](List(((1, 2), 1), ((2, 3), 2), ((1, 3), 5), ((4, 1), 4), ((4, 3), 3), ((5, 2), 5), ((5, 3), 5)))

    "Simple connected graph" should "be identified as connected" in {
        simpleGraph.isConnected should be (true)
    }

    it should "return vertices in correct order for DFS" in {
        (simpleGraph depthFirstIterate (1)) should be (List(1, 3, 4, 2))
    }

    it should "return correct minimum spanning tree" in {
        (simpleGraph getMinimumSpanningTree (1)).toSet should be (Set(((1, 2), 1), ((2, 3), 2), ((3, 4), 3)))
        (simpleGraph.getMinimumSpanningTree).toSet should be (Set(((1, 2), 1), ((2, 3), 2), ((3, 4), 3)))
    }

    "Simple directed graph" should "return correct order for DFS" in {
        (simpleDirectedGraph depthFirstIterate (1)) should be (List(1, 3, 4, 2))
    }

    it should "return correct strongly connected component" in {
        (simpleDirectedGraph getStronglyConnectedComponents) should be (Set(Set(1, 2, 3, 4)))
    }

    "Simple disconnected graph" should "return more than one strongly connected component" in {
        (simpleDisconnectedDirectedGraph depthFirstSearchReverse) should be (Vector(2, 1, 4, 3, 5))
        (simpleDisconnectedDirectedGraph getStronglyConnectedComponents) should be (Set(Set(1, 2, 3, 4), Set(5)))
    }

    "Kosaraju's algorithm for finding strongly connected components" should "return correct components" in {
        val lines = scala.io.Source.fromURL("file:///Users/venkat/Documents/Projects/Algorithms/src/test/resources/org/venustus/algorithms/graphs/SCC.txt").getLines.toList
        val graph =
            new DirectedGraph[Int](lines map ((s: String) => {
                val edgeElems: Array[String] = s.split(' ')
                Pair(Pair(edgeElems(0).toInt, edgeElems(1).toInt), 0)
            }))
        println("Graph has " + graph.numVertices + " vertices and " + graph.numEdges + " edges")
        val comps = graph.getStronglyConnectedComponents
        val sortedComps = comps.toList.sortWith(_.size > _.size)
        sortedComps.size should be (371762)
        sortedComps(0).size should be (434821)
        sortedComps(1).size should be (968)
        sortedComps(2).size should be (459)
        sortedComps(3).size should be (313)
        sortedComps(4).size should be (211)
    }

    it should "return correct components for medium sized graph" in {
        val lines = scala.io.Source.fromURL("file:///Users/venkat/Documents/Projects/Algorithms/src/test/resources/org/venustus/algorithms/graphs/SCC_test1.txt").getLines.toList
        val graph =
            new DirectedGraph[Int](lines map ((s: String) => {
                val edgeElems: Array[String] = s.split(' ')
                Pair(Pair(edgeElems(0).toInt, edgeElems(1).toInt), 0)
            }))
        println("Graph has " + graph.numVertices + " vertices and " + graph.numEdges + " edges")
        val comps = graph.getStronglyConnectedComponents
        val sortedComps = comps.toList.sortWith(_.size >= _.size)
        println(sortedComps)
        sortedComps.size should be (4)
        sortedComps(0).size should be (6)
        sortedComps(1).size should be (3)
        sortedComps(2).size should be (2)
        sortedComps(3).size should be (1)
    }

    "Decent sized graph" should "give correct cost for minimum spanning tree" in {
        val lines = scala.io.Source.fromURL("file:///Users/venkat/Documents/Projects/Algorithms/src/test/resources/org/venustus/algorithms/graphs/edges.txt").getLines.toList.tail
        val graph =
            new UndirectedGraph[Int](lines map ((s: String) => {
                val edgeElems: Array[String] = s.split(' ')
                Pair(Pair(edgeElems(0).toInt, edgeElems(1).toInt), edgeElems(2).toInt)
            }))
        println("Graph has " + graph.numVertices + " vertices and " + graph.numEdges + " edges")
        val minimumSpanningTreeCost = (0 /: graph.getMinimumSpanningTree)((acc: Int, edge: Graph.Edge[Int]) => acc + edge._2)
        println("Cost of minimum spanning tree: " + minimumSpanningTreeCost)
        minimumSpanningTreeCost should be (-3612829)

        val kruskalsMinimumSpanningTree = graph.getMinimumSpanningTree
        val kruskalsMinimumSpanningTreeCost = (0 /: kruskalsMinimumSpanningTree)((acc: Int, edge: Graph.Edge[Int]) => acc + edge._2)
        kruskalsMinimumSpanningTreeCost should be (-3612829)
    }

    "Simple graph" should "give correct all pairs shortest paths" in {
        val lines = scala.io.Source.fromURL("file:///Users/venkat/Documents/Projects/Algorithms/src/test/resources/org/venustus/algorithms/graphs/graph_apsp1.txt").getLines.toList.tail
        val graph =
            new DirectedGraph[Int](lines map ((s: String) => {
                val edgeElems: Array[String] = s.split(' ')
                Pair(Pair(edgeElems(0).toInt, edgeElems(1).toInt), edgeElems(2).toInt)
            }))
        println("Graph has " + graph.numVertices + " vertices and " + graph.numEdges + " edges")
        graph.getAllPairsShortestPaths.unzip._2.min should be (-1)
    }

    "Simple graph with negative cycle" should "throw NegativeCostCycleException" in {
        val lines = scala.io.Source.fromURL("file:///Users/venkat/Documents/Projects/Algorithms/src/test/resources/org/venustus/algorithms/graphs/graph_apsp2.txt").getLines.toList.tail
        val graph =
            new DirectedGraph[Int](lines map ((s: String) => {
                val edgeElems: Array[String] = s.split(' ')
                Pair(Pair(edgeElems(0).toInt, edgeElems(1).toInt), edgeElems(2).toInt)
            }))
        println("Graph has " + graph.numVertices + " vertices and " + graph.numEdges + " edges")
        intercept[NegativeCostCycleException] {
            graph.getAllPairsShortestPaths
        }
    }

    "Large graph 1" should "give correct all pairs shortest paths" in {
        val lines = scala.io.Source.fromURL("file:///Users/venkat/Documents/Projects/Algorithms/src/test/resources/org/venustus/algorithms/graphs/edges.txt").getLines.toList.tail
        val graph =
            new DirectedGraph[Int](lines map ((s: String) => {
                val edgeElems: Array[String] = s.split(' ')
                Pair(Pair(edgeElems(0).toInt, edgeElems(1).toInt), edgeElems(2).toInt)
            }))
        println("Graph has " + graph.numVertices + " vertices and " + graph.numEdges + " edges")
        graph.getAllPairsShortestPaths.unzip._2.min should be (-435795)
    }

    "Large graph 2" should "give correct all pairs shortest paths" in {
        val lines = scala.io.Source.fromURL("file:///Users/venkat/Documents/Projects/Algorithms/src/test/resources/org/venustus/algorithms/graphs/graph_apsp_g1.txt").getLines.toList.tail
        val graph =
            new DirectedGraph[Int](lines map ((s: String) => {
                val edgeElems: Array[String] = s.split(' ')
                Pair(Pair(edgeElems(0).toInt, edgeElems(1).toInt), edgeElems(2).toInt)
            }))
        println("Graph has " + graph.numVertices + " vertices and " + graph.numEdges + " edges")
        intercept[NegativeCostCycleException] {
            graph.getAllPairsShortestPaths.unzip._2.min
        }
    }

    "Large graph 3" should "give correct all pairs shortest paths" in {
        val lines = scala.io.Source.fromURL("file:///Users/venkat/Documents/Projects/Algorithms/src/test/resources/org/venustus/algorithms/graphs/graph_apsp_g2.txt").getLines.toList.tail
        val graph =
            new DirectedGraph[Int](lines map ((s: String) => {
                val edgeElems: Array[String] = s.split(' ')
                Pair(Pair(edgeElems(0).toInt, edgeElems(1).toInt), edgeElems(2).toInt)
            }))
        println("Graph has " + graph.numVertices + " vertices and " + graph.numEdges + " edges")
        intercept[NegativeCostCycleException] {
            graph.getAllPairsShortestPaths.unzip._2.min
        }
    }

    "Large graph 4" should "give correct all pairs shortest paths" in {
        val lines = scala.io.Source.fromURL("file:///Users/venkat/Documents/Projects/Algorithms/src/test/resources/org/venustus/algorithms/graphs/graph_apsp_g3.txt").getLines.toList.tail
        val graph =
            new DirectedGraph[Int](lines map ((s: String) => {
                val edgeElems: Array[String] = s.split(' ')
                Pair(Pair(edgeElems(0).toInt, edgeElems(1).toInt), edgeElems(2).toInt)
            }))
        println("Graph has " + graph.numVertices + " vertices and " + graph.numEdges + " edges")
        graph.getAllPairsShortestPaths.unzip._2.min should be (-19)
    }

    "Simple disconnected graph" should "have correct topological sort order" in {
        ((simpleDag getTopologicalSortOrdering) last) should be (3)
    }

    "Even tree algorithm" should "return correct number of edges to remove" in {
        val edgeList = List(((2, 1), 0), ((3, 1), 0), ((4, 3), 0), ((5, 2), 0), ((6, 1), 0), ((7, 2), 0), ((8, 6), 0), ((9, 8), 0), ((10, 8), 0))
        EvenTree.findMaxEdgesToRemoveForEvenForest(Tree(edgeList)) should be (2)

        val edgeList2 = List((2, 1), (3, 2), (4, 3), (5, 2), (6, 4), (7, 4), (8, 1), (9, 5), (10, 4), (11, 4),
            (12, 8), (13, 2), (14, 2), (15, 8), (16, 10), (17, 1), (18, 17), (19, 18), (20, 4), (21, 15), (22, 20),
            (23, 2), (24, 12), (25, 21), (26, 17), (27, 5), (28, 27), (29, 4), (30, 25)).map((x) => (x, 0))
        EvenTree.findMaxEdgesToRemoveForEvenForest(Tree(edgeList2)) should be (11)
    }
}
