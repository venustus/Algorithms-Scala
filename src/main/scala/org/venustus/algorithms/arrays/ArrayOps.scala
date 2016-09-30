package org.venustus.algorithms.arrays

import org.venustus.algorithms.arrays.ArrayOps.HeapElement
import org.venustus.algorithms.priorityqueues.PriorityQueue

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
     * We use the {@link reverseInPlace} sub-routine above to merge two sub-arrays
     * which are already partitioned according to the requirements.
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
            }
            else {
                val mid = s + ((e - s) / 2)
                partitionHelper(arr, s, mid, f)
                partitionHelper(arr, mid, e, f)
                var first: Int = s
                while(first < mid && f(arr(first))) first += 1
                var second: Int = mid
                while(second < e && f(arr(second))) second += 1
                if(first == mid || second == mid) {}
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
     * Algorithm (Kadene's algorithm):
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
            if (acc contains elem) acc - elem
            else acc + elem
        })
        s.head
    }

    /**
      * Find an element in a m x n matrix in which each row and
      * each column are in non-decreasing order.
      *
      * The trick here is that you can't start at left top or right bottom corner.
      * Because values are increasing in both ways, you don't know which way to go.
      *
      * But if you start at left bottom or right top, then values are decreasing
      * in one direction and increasing in another direction. So, you can always
      * take one or the other, depending on target element.
      *
      * Time complexity: O(m + n)
      *
      * @param arr
      * @param t
      * @tparam T
      * @return the coordinates of the element requested
      */
    def findElemInMatrix[T <% Ordered[T]](arr: Array[Array[T]], t: T): Option[(Int, Int)] = {
        def checkElem(x: Int, y: Int): Option[(Int, Int)] = {
            if(x < 0 || y >= arr(0).length) None
            else {
                if(arr(x)(y) == t) Some((x, y))
                else {
                    if(arr(x)(y) < t) checkElem(x, y + 1)
                    else checkElem(x - 1, y)
                }
            }
        }
        checkElem(arr.length - 1, 0)
    }

    /**
      * Finds and returns kth smallest element in a 2D array in which
      * each row and each column have elements in non-decreasing order.
      *
      * Note that if we could get all of the m*n elements into an array, we can
      * select kth smallest in O(mn) time, because selection problem is O(n) in worst case
      * (see Randomized Selection in CLRS). But this doesn't use the row/column-increasing
      * property of the matrix. So, it must be less than this.
      *
      * You can try to eliminate few portions of the matrix - for example if you take the matrix
      * at the right bottom containing (m-k) * (n-k) elements, that can surely not contain
      * the kth smallest because these are the biggest elements - but this is not leading to any
      * structural solution. We've got an oddly shaped matrix to look for, which is not convenient.
      * Further, k might be > m or > n (as long as it is < mn), so it may not be possible to pick
      * such a sub-matrix.
      *
      * Another thing that comes to mind when thinking about smallest elements is heap. We can easily
      * pick out smallest elements from a heap. When we turn the matrix 45 degrees clockwise, it sure
      * looks like a heap. From any given element, you can go either left or right and both are
      * greater than the element. But when you look carefully, it is not really a heap - because it
      * is not a complete binary tree. So, you'll have to construct the heap out of it. If you construct
      * a full heap of size O(mn), then its of no use because we could have used the original algorithm
      * in that case.
      *
      * But perhaps, we don't need to construct a heap of all the elements in the matrix. Let us try by
      * constructing heap from only first row. We pick out the smallest element and replace it with the one
      * in the same column as the one that got kicked out. This way, we are always successively picking
      * out smallest elements. When we do this exactly k times, whatever is left at the root of the heap
      * is the kth smallest.
      *
      * Time complexity: O(m + k log m)
      *
      * @param arr
      * @param k
      * @tparam T
      * @return
      */
    def findKthSmallestElementInMatrix[T <% Ordered[T]](arr: Array[Array[T]], k: Int): T = {
        val minHeap: PriorityQueue[HeapElement[T]] = PriorityQueue(arr(0).zipWithIndex map { case (e, i) => HeapElement(e, 0, i)})

        (minHeap /: (1 until k)) ((acc, _) => {
            val HeapElement(_, p, q) = minHeap.head
            minHeap.removeHead addElement HeapElement(arr(p + 1)(q), p + 1, q)
        }).head.elem
    }

    case class HeapElement[P <% Ordered[P]](elem: P, i: Int, j: Int) extends Ordered[HeapElement[P]] {
        override def compare(that: HeapElement[P]): Int = {
            that.elem.compare(this.elem)
        }
    }
}
