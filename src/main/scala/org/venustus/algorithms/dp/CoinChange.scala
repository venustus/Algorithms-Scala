package org.venustus.algorithms.dp

/**
 * Created by venkat on 16/06/15.
 */
object CoinChange {

    /**
     * Given an integer n and a list of denominations available,
     * find number of ways in which a change for n dollars can be obtained, by
     * combining different quantities of coins of each type. All coins are available
     * in infinite amount.
     *
     * Algorithm:
     *
     * This can be solved in linear time using dynamic programming approach.
     *
     * Optimal Substructure:
     *
     * Consider the ith coin. We may either use ith coin in your final answer or not.
     * If we have used the ith coin, then we may use it once or more than once. Let us say we have used
     * ith coin j times. Then total number of ways of making up n using just first i coins is:
     * f(i - 1, n) + f(i - 1, n - C(i)) + f(i - 1, n - 2 * C(i)) + ... + f(i - 1, n - j * C(i))
     * where n >= j * C(i).
     *
     * Base cases: f(0, _) = 0, f(_, 0) = 0
     *
     * We can convert this recursive definition into an iterative algorithm, by computing f(i) for each
     * i from 0 through n.
     *
     * Time complexity: O(n * m) where n is given integer and m is number of coin denominations available.
     *
     * @param n
     * @param denominations
     */
    def getNumberOfWaysOfMakingChange(n: Int, denominations: Vector[Int]) = {
        val table = Array.fill[Long](denominations.size + 1, n + 1)(0)
        for(i <- 0 to denominations.size) {
            table(i)(0) = 1
        }
        (table /: denominations.zipWithIndex)((trackingTable, denom) => {
            (trackingTable /: (1 to n))((trackingTable2, k) => {
                var j = 0
                while(k >= j * denom._1) {
                    trackingTable2(denom._2 + 1)(k) += trackingTable2(denom._2)(k - j * denom._1)
                    j = j + 1
                }
                trackingTable2
            })
        })
        table(denominations.size)(n)
    }

}
