package org.venustus.algorithms.npc

import org.scalatest.{Matchers, FlatSpec}

/**
 * Created by venkat on 16/08/14.
 */
class TSPSpec extends FlatSpec with Matchers {
    "DP solution to TSP" should "find correct optimal tour for small datasets" in {
        val lines = scala.io.Source.fromURL("file:///Users/venkat/Documents/Projects/Algorithms/src/test/resources/org/venustus/algorithms/npc/tsp1.txt").getLines.toList.tail
        val cities = lines map ((s: String) => {
            val edgeElems: Array[String] = s.split(' ')
            Pair(edgeElems(0).toFloat, edgeElems(1).toFloat)
        })
        val tsp = new TSP(cities.toVector)
        tsp.getOptimalTourCost should be (4.0)

        val lines2 = scala.io.Source.fromURL("file:///Users/venkat/Documents/Projects/Algorithms/src/test/resources/org/venustus/algorithms/npc/tsp2.txt").getLines.toList.tail
        val cities2 = lines2 map ((s: String) => {
            val edgeElems: Array[String] = s.split(' ')
            Pair(edgeElems(0).toFloat, edgeElems(1).toFloat)
        })
        val tsp2 = new TSP(cities2.toVector)
        (tsp2.getOptimalTourCost - 10.472136) should be < 0.0001
    }

    it should "find correct optimal tour for medium data sets" in {
        val lines = scala.io.Source.fromURL("file:///Users/venkat/Documents/Projects/Algorithms/src/test/resources/org/venustus/algorithms/npc/tsp3.txt").getLines.toList.tail
        val cities = lines map ((s: String) => {
            val edgeElems: Array[String] = s.split(' ')
            Pair(edgeElems(0).toFloat, edgeElems(1).toFloat)
        })
        val tsp = new TSP(cities.toVector)
        (tsp.getOptimalTourCost - 16898.13) should be < 0.01

        val lines2 = scala.io.Source.fromURL("file:///Users/venkat/Documents/Projects/Algorithms/src/test/resources/org/venustus/algorithms/npc/tsp4.txt").getLines.toList.tail
        val cities2 = lines2 map ((s: String) => {
            val edgeElems: Array[String] = s.split(' ')
            Pair(edgeElems(0).toFloat, edgeElems(1).toFloat)
        })
        val tsp2 = new TSP(cities2.toVector)
        (tsp2.getOptimalTourCost - 26714.9) should be < 0.1
    }

    it should "find correct optimal tour for large data sets" in {
        val lines = scala.io.Source.fromURL("file:///Users/venkat/Documents/Projects/Algorithms/src/test/resources/org/venustus/algorithms/npc/tsp5.txt").getLines.toList.tail
        val cities = lines map ((s: String) => {
            val edgeElems: Array[String] = s.split(' ')
            Pair(edgeElems(0).toFloat, edgeElems(1).toFloat)
        })
        val tsp = new TSP(cities.toVector)
        println(tsp.getOptimalTourCost); // - 16898.13) should be < 0.01
    }
}
