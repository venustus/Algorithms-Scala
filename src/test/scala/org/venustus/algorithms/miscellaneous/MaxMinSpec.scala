package org.venustus.algorithms.miscellaneous

import org.scalatest.{Matchers, FlatSpec}

/**
 * Created by venkat on 03/05/15.
 */
class MaxMinSpec extends FlatSpec with Matchers {
    "MaxMin" should "find minimum difference" in {
        MaxMin.findMinimumUnfairness(Vector[Int](10, 100, 300, 200, 1000, 20, 30), 3) should be (20)
        MaxMin.findMinimumUnfairness(Vector[Int](1, 2, 3, 4, 10, 20, 30, 40, 100, 200), 4) should be (3)
        MaxMin.findMinimumUnfairness(Vector[Int](10, 20, 30, 100, 101, 102), 3) should be (2)
    }
}
