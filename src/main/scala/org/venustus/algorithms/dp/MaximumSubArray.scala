package org.venustus.algorithms.dp

/**
 * Created by venkat on 26/05/15.
 */
object MaximumSubArray {

    /**
     * Returns maximum sum contiguous sub array of a given array <code>ls</code>.
     *
     * Algorithm: Kadene's algorithm
     *
     * We run one single loop over the input array. We keep track of 5 values as we loop
     * through the array. <code>currentSum</code> represents the currently tracked sum (local maxima).
     * <code>currentStart</code> represents the start index for the local maxima.
     * <code>bestSum</code>, <code>bestStart</code> and <code>bestEnd</code> represents the ultimate solution.
     *
     * For each element, we consider the following cases:
     * 1) The element itself is greater than the current sum (after adding the current element). This could
     *    mean the current sum is in negative and hence we should start considering a fresh sub array. We also
     *    update the best sum if needed.
     * 2) When the current element is added, the current sum exceeds the best so far. Then update the best.
     * 3) When the current element is added, the new sum is better than the current sum so far and also the current element.
     *    In this case, add the current element to the local maxima.
     * 4) Otherwise, we start considering a fresh array.
     *
     * Time complexity: O(n)
     *
     * @param ls
     * @return
     */
    def findMaxContiguousSubArray(ls: Array[Int]) = {
        val ans = ((0, Int.MinValue, 0, -1, -1) /: (ls zipWithIndex))((acc, ith) => {
            acc match {
                case (currentSum, bestSum, currentStart, bestStart, bestEnd) => {
                    if(ith._1 > currentSum + ith._1) (ith._1, math.max(ith._1, bestSum), ith._2, if(ith._1 > bestSum) ith._2 else bestStart, if(ith._1 > bestSum) ith._2 else bestEnd)
                    else if(currentSum + ith._1 > bestSum) (currentSum + ith._1, currentSum + ith._1, currentStart, currentStart, ith._2)
                    else {
                        if(currentSum + ith._1 > currentSum || currentSum + ith._1 > ith._1) (currentSum + ith._1, bestSum, currentStart, bestStart, bestEnd)
                        else (ith._1, bestSum, ith._2, bestStart, bestEnd)
                    }
                }
            }
        })
        ls.slice(ans._4, ans._5 + 1)
    }

    /**
     * Computes and returns the maximum sub array that is not necessarily contiguous.
     *
     * Algorithm:
     *
     * We simply loop over the array tracking just two things - the positive sum and maximum negative number.
     *
     * If all the numbers are negative, then we return the maximum negative number. Otherwise, there is
     * at least one positive number. Then we just return the sum of all positive numbers.
     *
     * @param ls
     * @return
     */
    def findMaxNonContiguousSubArray(ls: Array[Int]) = {
        val ans = ((0, true, Int.MinValue) /: (ls zipWithIndex))((acc, ith) => {
            acc match {
                case (positiveSum, allNegative, maxNegativeNum) => {
                    if(ith._1 > 0) (positiveSum + ith._1, allNegative && false, maxNegativeNum)
                    else (positiveSum, allNegative && true, math.max(maxNegativeNum, ith._1))
                }
            }
        })
        if(ans._2) ans._3
        else ans._1
    }

}
