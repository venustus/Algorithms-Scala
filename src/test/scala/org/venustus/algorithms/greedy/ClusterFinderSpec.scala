package org.venustus.algorithms.greedy

import org.scalatest.{FlatSpec, Matchers}
import org.venustus.algorithms.graphs.{Graph, UndirectedGraph}
import org.venustus.algorithms.unionfind.{OptimizedUnionFind, UnionFind}

/**
 * Created by venkat on 20/07/14.
 */
class ClusterFinderSpec extends FlatSpec with Matchers {
    "Cluster finder" should "find correct clusters" in {
        val cf = new ClusterFinder
        cf.findKClusters(List(((1, 2), 1), ((2, 3), 2), ((3, 4), 3), ((4, 1), 4), ((1, 3), 5), ((2, 4), 6)), 2) should be (3)
    }

    it should "find correct clusters in a medium graph" in {
        val cf = new ClusterFinder
        val lines = scala.io.Source.fromURL("file:///Users/venkat/Documents/Projects/Algorithms/src/test/resources/org/venustus/algorithms/greedy/clustering2.txt").getLines.toList.tail
        val edges = lines map ((s: String) => {
            val edgeElems: Array[String] = s.split(' ')
            Pair(Pair(edgeElems(0).toInt, edgeElems(1).toInt), edgeElems(2).toInt)
        })
        cf.findKClusters(edges, 4) should be (134365)
    }

    it should "find correct clusters in a complex graph" in {
        val cf = new ClusterFinder
        val lines = scala.io.Source.fromURL("file:///Users/venkat/Documents/Projects/Algorithms/src/test/resources/org/venustus/algorithms/greedy/clustering1.txt").getLines.toList.tail
        val edges = lines map ((s: String) => {
                val edgeElems: Array[String] = s.split(' ')
                Pair(Pair(edgeElems(0).toInt, edgeElems(1).toInt), edgeElems(2).toInt)
            })
       cf.findKClusters(edges, 4) should be (106)
    }

    it should "find correct clusters in a very big graph" in {
        val cf = new ClusterFinder
        val lines = scala.io.Source.fromURL("file:///Users/venkat/Documents/Projects/Algorithms/src/test/resources/org/venustus/algorithms/greedy/clustering3.txt").getLines.toList
        val size: Array[String] = lines.head.split(" ")
        val nodes: List[Int] = (lines.tail map ((s: String) => Integer.parseInt(s.replace(" ", ""), 2)))
        cf.findNumClustersForMinimumSpacing(nodes, Integer.parseInt(size(1))) should be (1)

        val lines2 = scala.io.Source.fromURL("file:///Users/venkat/Documents/Projects/Algorithms/src/test/resources/org/venustus/algorithms/greedy/clustering4.txt").getLines.toList
        val size2: Array[String] = lines2.head.split(" ")
        val nodes2: List[Int] = (lines2.tail map ((s: String) => Integer.parseInt(s.replace(" ", ""), 2)))
        cf.findNumClustersForMinimumSpacing(nodes2, Integer.parseInt(size2(1))) should be (3)

        val lines3 = scala.io.Source.fromURL("file:///Users/venkat/Documents/Projects/Algorithms/src/test/resources/org/venustus/algorithms/greedy/clustering5.txt").getLines.toList
        val size3: Array[String] = lines3.head.split(" ")
        val nodes3: List[Int] = (lines3.tail map ((s: String) => Integer.parseInt(s.replace(" ", ""), 2)))
        cf.findNumClustersForMinimumSpacing(nodes3, Integer.parseInt(size3(1))) should be (11)

        val lines4 = scala.io.Source.fromURL("file:///Users/venkat/Documents/Projects/Algorithms/src/test/resources/org/venustus/algorithms/greedy/clustering_big.txt").getLines.toList
        val size4: Array[String] = lines4.head.split(" ")
        val nodes4: List[Int] = (lines4.tail map ((s: String) => Integer.parseInt(s.replace(" ", ""), 2)))
        cf.findNumClustersForMinimumSpacing(nodes4, Integer.parseInt(size4(1))) should be (6118)
    }
}
