package org.venustus.algorithms.npc

import org.scalatest.{Matchers, FlatSpec}
import org.venustus.algorithms.graphs.DirectedGraph

/**
 * Created by venkat on 24/08/14.
 */
class TwoSatSpec extends FlatSpec with Matchers {
    "2-SAT instance 1" should "recognize feasibility correctly" in {
        val lines = scala.io.Source.fromURL("file:///Users/venkat/Documents/Projects/Algorithms/src/test/resources/org/venustus/algorithms/npc/2sat1.txt").getLines.toList.tail
        println("2-SAT instance 1 feasibility: " + TwoSat.isTwoSatisfiable(lines map ((s: String) => {
            val edgeElems: Array[String] = s.split(' ')
            Pair(edgeElems(0).toInt, edgeElems(1).toInt)
        })))
    }

    "2-SAT instance 2" should "recognize feasibility correctly" in {
        val lines = scala.io.Source.fromURL("file:///Users/venkat/Documents/Projects/Algorithms/src/test/resources/org/venustus/algorithms/npc/2sat2.txt").getLines.toList.tail
        println("2-SAT instance 2 feasibility: " + TwoSat.isTwoSatisfiable(lines map ((s: String) => {
            val edgeElems: Array[String] = s.split(' ')
            Pair(edgeElems(0).toInt, edgeElems(1).toInt)
        })))
    }

    "2-SAT instance 3" should "recognize feasibility correctly" in {
        val lines = scala.io.Source.fromURL("file:///Users/venkat/Documents/Projects/Algorithms/src/test/resources/org/venustus/algorithms/npc/2sat3.txt").getLines.toList.tail
        println("2-SAT instance 3 feasibility: " + TwoSat.isTwoSatisfiable(lines map ((s: String) => {
            val edgeElems: Array[String] = s.split(' ')
            Pair(edgeElems(0).toInt, edgeElems(1).toInt)
        })))
    }

    "2-SAT instance 4" should "recognize feasibility correctly" in {
        val lines = scala.io.Source.fromURL("file:///Users/venkat/Documents/Projects/Algorithms/src/test/resources/org/venustus/algorithms/npc/2sat4.txt").getLines.toList.tail
        println("2-SAT instance 4 feasibility: " + TwoSat.isTwoSatisfiable(lines map ((s: String) => {
            val edgeElems: Array[String] = s.split(' ')
            Pair(edgeElems(0).toInt, edgeElems(1).toInt)
        })))
    }

    "2-SAT instance 5" should "recognize feasibility correctly" in {
        val lines = scala.io.Source.fromURL("file:///Users/venkat/Documents/Projects/Algorithms/src/test/resources/org/venustus/algorithms/npc/2sat5.txt").getLines.toList.tail
        println("2-SAT instance 5 feasibility: " + TwoSat.isTwoSatisfiable(lines map ((s: String) => {
            val edgeElems: Array[String] = s.split(' ')
            Pair(edgeElems(0).toInt, edgeElems(1).toInt)
        })))
    }

    "2-SAT instance 6" should "recognize feasibility correctly" in {
        val lines = scala.io.Source.fromURL("file:///Users/venkat/Documents/Projects/Algorithms/src/test/resources/org/venustus/algorithms/npc/2sat6.txt").getLines.toList.tail
        println("2-SAT instance 6 feasibility: " + TwoSat.isTwoSatisfiable(lines map ((s: String) => {
            val edgeElems: Array[String] = s.split(' ')
            Pair(edgeElems(0).toInt, edgeElems(1).toInt)
        })))
    }
}
