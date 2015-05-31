package org.venustus.algorithms.dp

import org.scalatest.{FlatSpec, Matchers}

/**
 * Created by venkat on 26/05/15.
 */
class MaximumSubArraySpec extends FlatSpec with Matchers {
    "Function for finding maximum contiguous sub array" should "return correct sub array" in {
        MaximumSubArray.findMaxContiguousSubArray(Array(1, 2, 3, 4)) should be (Array(1, 2, 3, 4))
        MaximumSubArray.findMaxContiguousSubArray(Array(2, -1, 2, 3, 4, -5)) should be (Array(2, -1, 2, 3, 4))
        MaximumSubArray.findMaxContiguousSubArray(Array(-2, -3, -1, -5, -4)) should be (Array(-1))
        MaximumSubArray.findMaxContiguousSubArray(Array(2, -1, 2, 3, 4, -5, 100)) should be (Array(2, -1, 2, 3, 4, -5, 100))
        MaximumSubArray.findMaxContiguousSubArray(Array(2, -1, 2, 3, 4, -50, 100, -50)) should be (Array(100))
    }

    "Function for finding maximum non-contiguous sub array" should "return correct sub array" in {
        MaximumSubArray.findMaxNonContiguousSubArray(Array(2, -1, 2, 3, 4, -5)) should be (11)
    }
}
