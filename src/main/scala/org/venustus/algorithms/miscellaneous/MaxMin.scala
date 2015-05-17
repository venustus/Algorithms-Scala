package org.venustus.algorithms.miscellaneous

/**
 * Created by venkat on 03/05/15.
 */
object MaxMin {
    /**
     * Finds a subset of size k (x1, x2 .. xk), of a given vector of size n such that
     * the unfairness Max(x1, x2 ... xk) - Min(x1, x2, .. xk) is minimized.
     *
     * Algorithm:
     *
     * 1) First sort the given vector according to natural ordering O(n log n)
     * 2) Now, there are only n - k possible sequences to check. The key to the solution is the observation that
     *    in the sorted array, the subset whose unfairness is minimum must appear consecutively. To observe this,
     *    assume the contradiction - Let us say x1, x2 .. xi-1, xi+1 .. xk+1 is a minimum unfairness subset.
     *    Then we can always drop xk+1 and include xi in the chosen subset of size k and we'll obtain even lower
     *    unfairness subset - which is contradicting our assumption.
     *
     * 3) So, we start with index k - 1 and check until n - 1 and keep a track of the difference between
     *    maximum and minimum at each index (since the list is now sorted, these are nothing but last and first
     *    numbers in the range) O(n)
     *
     * Overall Time complexity: O(n)
     * Overall Space complexity: O(1)
     *
     * @param ls
     * @param k
     * @return
     */
    def findMinimumUnfairness(ls: Vector[Int], k: Int): Int = {
        val sortedList = ls.sorted
        var unfairness: Int = Int.MaxValue
        var minSoFar: Int = Int.MaxValue
        var i: Int = 0
        for(i <- 0 to (ls.size - 1)) {
            minSoFar = if (sortedList(i) < minSoFar) sortedList(i) else minSoFar
            if (i >= k - 1) {
                val currentDiff = sortedList(i) - sortedList(i - k + 1)
                unfairness = if(currentDiff < unfairness) currentDiff else unfairness
            }
        }
        unfairness
    }
}
