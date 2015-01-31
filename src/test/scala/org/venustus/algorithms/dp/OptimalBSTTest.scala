package org.venustus.algorithms.dp

import org.scalatest.{FlatSpec, Matchers}

/**
 * Created by venkat on 26/07/14.
 */
class OptimalBSTTest extends FlatSpec with Matchers{
    "OptimalBST" should "compute correct optimal search time" in {
        OptimalBST.computeOptimalBST(Vector(0.05, 0.4, 0.08, 0.04, 0.1, 0.1, 0.23))
        OptimalBST.computeOptimalBST(Vector(0.2, 0.05, 0.17, 0.1, 0.2, 0.03, 0.25))
    }
}
