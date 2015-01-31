package org.venustus.algorithms.dp

import org.scalatest.{FlatSpec, Matchers}

/**
 * Created by venkat on 11/01/15.
 */
class SubsetWithSumSpec extends FlatSpec with Matchers {
    "The algorithm for retrieving subset with given sum" should "work correctly on boundary cases" in {
        SubsetsWithSum.getSubsetWithSum(Array(1), 1) should be (Some(Set(1)))
        SubsetsWithSum.getSubsetWithSum(Array(1, 2), 3) should be (Some(Set(1, 2)))
        SubsetsWithSum.getSubsetWithSum(Array(1, 2), 1) should be (Some(Set(1)))
        SubsetsWithSum.getSubsetWithSum(Array(1, 2), 2) should be (Some(Set(2)))
        SubsetsWithSum.getSubsetWithSum(Array(1, 2), 4) should be (None)
    }
}
