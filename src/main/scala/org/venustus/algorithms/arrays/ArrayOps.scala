package org.venustus.algorithms.arrays

/**
 * Created by venkat on 27/08/14.
 */
object ArrayOps {
    /**
     * Reverses an array in-place.
     *
     * Time complexity: O(n)
     * Space complexity: O(1)
     * @param arr
     * @param start
     * @param end
     */
    def reverseInPlace(arr: Array[Int], start: Int, end: Int) = {
        var s = start
        var e = end - 1
        while(s < e) {
            val tmp = arr(s)
            arr(s) = arr(e)
            arr(e) = tmp
            s += 1
            e -= 1
        }
    }

    /**
     * Partitions a given array according to given partition function, in-place.
     *
     * Time complexity: O(n log n)
     * Space complexity: O(1)
     * @param arr
     * @param f
     */
    def partition(arr: Array[Int], f: (Int) => Boolean): Unit = {
        def partitionHelper(arr: Array[Int], s: Int, e: Int, f: (Int) => Boolean): Unit = {
            if(e - s == 2) {
                if(!f(arr(s)) && f(arr(e - 1))) {
                    val tmp = arr(s)
                    arr(s) = arr(e - 1)
                    arr(e - 1) = tmp
                }
            }
            else if(e - s == 1) {
                return
            }
            else {
                val mid = s + ((e - s) / 2)
                partitionHelper(arr, s, mid, f)
                partitionHelper(arr, mid, e, f)
                var first: Int = s
                while(first < mid && f(arr(first))) first += 1
                var second: Int = mid
                while(second < e && f(arr(second))) second += 1
                if(first == mid || second == mid) return
                else {
                    reverseInPlace(arr, first, second)
                    reverseInPlace(arr, first, first + (second - mid))
                    reverseInPlace(arr, first + (second - mid), second)
                }
            }
        }
        partitionHelper(arr, 0, arr.length, f)
    }


    /**
     * Problem:
     * Given a list of projected stock prices, one stock price for each day,
     * find out on which day to buy the stock and on which day to sell, to
     * make maximum profit.
     *
     * Alternative statement:
     * Given an integer array A, find out indexes i and j such that A[i] - A[j]
     * is maximum while i > j.
     *
     * Algorithm:
     * 1) Iterate once through the array.
     * 2) At each iteration, keep a running minimum seen so far.
     * 3) At each iteration, take the difference of the current element and
     *    the minimum stock price seen so far. Keep a running max profit
     *    and if the current profit is greater than max profit, update max profit.
     *    Also update start and end indexes.
     * 4) Return max profit and start and end indexes.
     *
     * Time complexity: O(n)
     * Space complexity: O(1)
     */
    def findMaxProfit(stocks: List[Int]) = {
        val(_, _, _, start, end, _) = ((Int.MaxValue, -1, Int.MinValue, -1, -1, 0) /: stocks)((acc, stockPrice) => {
            val (minSoFar, minIndexSoFar, maxProfit, start, end, curIndex) = acc
            val (updatedMinSoFar, updatedMinIndexSoFar) =
                if(stockPrice < minSoFar) (stockPrice, curIndex) else (minSoFar, minIndexSoFar)

            val (updatedMaxProfit, newStart, newEnd) =
                if(minIndexSoFar >= 0) {
                    if(stockPrice - minSoFar > maxProfit) (stockPrice - minSoFar, minIndexSoFar, curIndex)
                    else (maxProfit, start, end)
                }
                else (0, curIndex, curIndex)
            (updatedMinSoFar, updatedMinIndexSoFar, updatedMaxProfit, newStart, newEnd, curIndex + 1)
        })
        (start, end)
    }

    /**
     * Retrieves the sub array with largest sum in a given integer array.
     * Algorithm:
     * 1) Start with first element and do a linear sweep of the array.
     * 2) At each step, compute and keep track of the maximum sum of any sub-array
     *    ending at the current position. max(i) can be computed based on max(i-1) as follows:
     *    2.1) If max(i-1) is negative, then we don't need its contribution. Set max(i) to just a[i].
     *    2.2) Otherwise, set max(i) to a[i] + max(i).
     * 3) At each step keep track of a running maxSoFar (and its indexes). If max(i) exceeds
     *    maxSoFar at any point, reset maxSoFar to max(i). Also reset the indexes.
     *
     * Time complexity: O(n)
     * Space complexity: O(1)
     */
    def getLargestSumContiguousSubsequence(ls: Seq[Int]) = {
        val (finalMax, _, finalStart, finalEnd, _) = ((Int.MinValue, Int.MinValue, -1, -1, 0) /: ls)((acc, i) => {
            val(maxSoFar, prevMax, start, end, curIndex) = acc
            val (currentMax, startNew, endNew) = if(prevMax < 0) (i, curIndex, curIndex) else (prevMax + i, start, curIndex)
            if(currentMax > maxSoFar) (currentMax, currentMax, startNew, endNew, curIndex + 1)
            else (maxSoFar, currentMax, start, end, curIndex + 1)
        })
        (finalStart, finalEnd, finalMax)
    }

    /**
     * Given a list of elements in which all elements except one, occur in pairs,
     * finds out the one that doesn't have a pair.
     *
     * Algorithm:
     * 1) Create an empty set.
     * 2) Loop through the elements in the list and check if each element is already in the set.
     * 3) If already in the set, remove from the set, otherwise add it.
     * 4) At the end of the loop, this set should contain the lone element.
     *
     * Time Complexity: O(n)
     * Space Complexity: O(n)
     *
     * @param ls
     * @tparam T
     * @return
     */
    def findLoneElement[T](ls: List[T]): T = {
        val s = (Set[T]() /: ls)((acc, elem) => {
            if (acc contains (elem)) acc - elem
            else acc + elem
        })
        s.head
    }
}
