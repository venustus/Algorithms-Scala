package org.venustus.algorithms.search

import org.scalatest.{FlatSpec, Matchers}

/**
 * Created by venkat on 03/06/15.
 */
class ConnectedGridCellsSpec extends FlatSpec with Matchers {
    "Algorithm to find largest connected region" should "find largest region correctly" in {
        ConnectedGridCells.findLargestConnectedRegion(Array(Array(1, 1, 0, 0), Array(0, 1, 1, 0), Array(0, 0, 1, 0), Array(1, 0, 0, 0))) should be (5)
        ConnectedGridCells.findLargestConnectedRegion(Array(Array(0, 0, 1, 1), Array(0, 0, 1, 0), Array(0, 1, 1, 0), Array(0, 1, 0, 0), Array(1, 1, 0, 0))) should be (8)
        ConnectedGridCells.findLargestConnectedRegion(Array(Array(0, 1, 1, 1, 1), Array(1, 0, 0, 0, 1), Array(1, 1, 0, 1, 0), Array(0, 1, 0, 1, 1), Array(0, 1, 1, 1, 0))) should be (15)
    }
}
