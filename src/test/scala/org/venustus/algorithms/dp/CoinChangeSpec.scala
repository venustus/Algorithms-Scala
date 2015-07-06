package org.venustus.algorithms.dp

import org.scalatest.{FlatSpec, Matchers}

/**
 * Created by venkat on 16/06/15.
 */
class CoinChangeSpec extends FlatSpec with Matchers {

    "Coin change algorithm" should "compute correct number of ways to make change" in {
        CoinChange.getNumberOfWaysOfMakingChange(4, Vector[Int](1, 2, 3)) should be (4)
        CoinChange.getNumberOfWaysOfMakingChange(10, Vector[Int](2, 5, 3, 6)) should be (5)
        CoinChange.getNumberOfWaysOfMakingChange(250, Vector[Int](41, 34, 46, 9, 37, 32, 42, 21, 7, 13, 1, 24, 3, 43, 2, 23, 8, 45, 19, 30, 29, 18, 35, 11)) should be (15685693751L)
    }

}
