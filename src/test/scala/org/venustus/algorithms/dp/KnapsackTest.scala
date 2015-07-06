package org.venustus.algorithms.dp

import org.scalatest.{FlatSpec, Matchers}

/**
 * Created by venkat on 27/07/14.
 */
class KnapsackTest extends FlatSpec with Matchers {
    "A DP solution for knapsack" should "compute correct max value solution" in {
        val res01 = Knapsack.computeOptimalSolution("file:///Users/venkat/Documents/Projects/Algorithms/src/test/resources/org/venustus/algorithms/dp/knapsack01.txt", true)
        res01 should be (9)
        val res1 = Knapsack.computeOptimalSolution("file:///Users/venkat/Documents/Projects/Algorithms/src/test/resources/org/venustus/algorithms/dp/knapsack1.txt")
        res1 should be (2493893)
        val res2 = Knapsack.computeOptimalSolution("file:///Users/venkat/Documents/Projects/Algorithms/src/test/resources/org/venustus/algorithms/dp/knapsack1.txt", true)
        res2 should be (res1)
        val res3 = Knapsack.computeOptimalSolution("file:///Users/venkat/Documents/Projects/Algorithms/src/test/resources/org/venustus/algorithms/dp/knapsack_big.txt", true)
        println(res3)
    }
}
