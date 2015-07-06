package org.venustus.algorithms.dp

import org.scalatest.{FlatSpec, Matchers}

/**
 * Created by venkat on 17/06/15.
 */
class StockMaximizeSpec extends FlatSpec with Matchers {

    "Stock maximization function" should "compute correct maximum profit" in {
        StockMaximize.maximizeProfit(Vector[Long](5, 3, 2)) should be (0)
        StockMaximize.maximizeProfit(Vector[Long](1, 2)) should be (1)
        StockMaximize.maximizeProfit(Vector[Long](1, 2, 100)) should be (197)
        StockMaximize.maximizeProfit(Vector[Long](1, 3, 1, 2)) should be (3)
    }

}
