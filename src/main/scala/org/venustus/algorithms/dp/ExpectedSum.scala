package org.venustus.algorithms.dp

/**
 * Created by venkat on 12/07/15.
 */
object ExpectedSum {

    def findMaxExpectedSum(ls: List[Int], limit: Int): Int = {
        ls match {
            case h :: t => (0 /: (0 to (limit / h)))((acc, l) => {
                val subSum = h * l + findMaxExpectedSum(t, limit - (h * l))
                math.max(acc, subSum)
            })
            case Nil => 0
        }
    }

    /**
     * Find maximum expected sum while picking 0 or more occurrences of each item from a given list,
     * while the sum is upper bounded by a limit.
     *
     * Approach: Dynamic Programming
     *
     * Basic recursive relation is given by:
     *
     * maxSum[s] = max among all i (ls[i] + maxSum[s - ls[i]])
     *
     * @param ls
     * @param limit
     * @return
     */
    def findMaxExpectedSumEfficient(ls: List[Int], limit: Int): Int = {
        val table = (Vector.fill[Int](limit + 1)(0) /: (1 to limit))((acc, sum) => {
            acc updated (sum, (0 /: ls)((acc2, item) => {
                if(item > sum) acc2
                else math.max(acc2, item + acc(sum - item))
            }))
        })
        table(limit)
    }

}
