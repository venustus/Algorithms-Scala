package org.venustus.algorithms.graphs

import org.scalatest.{FlatSpec, Matchers}

/**
 * Created by venkat on 27/06/15.
 */
class ProbabilityOfReachSpec extends FlatSpec with Matchers {

    "Function for computing probability of reach" should "compute correct probability" in {
        val g = new DirectedGraph[Int](List(((1, 2), 0), ((1, 3), 0), ((2, 3), 0), ((3, 4), 0), ((4, 2), 0)))
        ProbabilityOfReach.findProbabilityOfReach(g, 4, Map(1 -> 0.45, 2 -> 0.23, 3 -> 0.32)).toInt should be (1)
        val h = new DirectedGraph[Int](List(((1, 2), 0), ((1, 3), 0), ((3, 2), 0), ((3, 4), 0), ((4, 2), 0)))
        ProbabilityOfReach.findProbabilityOfReach(h, 4, Map(1 -> 0.45, 2 -> 0.23, 3 -> 0.32)) should be (0.77)
    }

}
