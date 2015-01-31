package org.venustus.algorithms.dp

/**
 * Created by venkat on 11/01/15.
 */
object SubsetsWithSum {

    /**
     * Problem:
     * Given an array of non-negative integers, find a subset for which
     * sum of all integers in the set equals a given value k.
     *
     * Approach: Dynamic Programming
     * Algorithm:
     * 1) Initialize a 2D array m such that m[i][j] contains a pointer to a subset
     *    whose sum is j or NULL if no such subset exists including only elements
     *    from 0 through i in array 'arr'.
     * 2) Set m[i][arr[i]] = {arr[i]} for all i from 0 through n - 1
     * 3) Recurrence relation: m[i][sum] = either m[i - 1][sum] or {m[i - 1][sum - arr[i]], arr[i]}
     * 4) Compute m bottom up, by having two loops one on 'sum' going from 1 through k and another
     *    on the input array going from 1 through arr->size().
     * 5) At the end, return m[arr->size() - 1][k]
     *
     * Time complexity: O(k*n)
     * Space complexity: O(k*n)
     *
     * Note: This problem is NP-complete. Because 'k' is exponential in n.
     */
    def getSubsetWithSum(ls: IndexedSeq[Int], k: Int) = {
        val m = (Array.fill[Option[Set[Int]]](ls.size, k + 1) { None } /: (0 until ls.size))((acc, index) => {
            if(ls(index) <= k) acc(index)(ls(index)) = Some(Set(ls(index)))
            acc(index)(0) = Some(Set())
            acc
        })
        for { i <- 1 until ls.size; j <- 1 to k }
            m(i)(j) = if(j >= ls(i) && m(i - 1)(j - ls(i)) != None) {
                          val Some(x) = m(i - 1)(j - ls(i))
                          Some(x + ls(i))
                      }
                      else m(i - 1)(j)
        m(ls.size - 1)(k)
    }
}
