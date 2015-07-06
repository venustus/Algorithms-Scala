package org.venustus.algorithms.sorting

import org.scalatest.{FlatSpec, Matchers}

/**
 * Created by venkat on 05/06/15.
 */
class AlmostSortedSpec extends FlatSpec with Matchers {

    "Algorithm for almost sorted problem" should "find correct swap/reverse" in {
        AlmostSorted.isAlmostSorted(Array(4, 2)) should be (Some("swap 1 2"))
        AlmostSorted.isAlmostSorted(Array(1, 2, 3, 4, 5)) should be (Some("yes"))
        AlmostSorted.isAlmostSorted(Array(3, 1, 2)) should be (None)
        AlmostSorted.isAlmostSorted(Array(2, 1, 3)) should be (Some("swap 1 2"))
        AlmostSorted.isAlmostSorted(Array(1, 5, 4, 3, 2, 6)) should be (Some("reverse 2 5"))
    }

}
