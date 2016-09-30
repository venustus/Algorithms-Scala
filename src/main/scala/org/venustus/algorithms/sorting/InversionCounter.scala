package org.venustus.algorithms.sorting

import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag

/**
  * Created by venkat on 18/09/16.
  */
object InversionCounter {


    /**
      * Counts number of inversions in a given array. An inversion is a pair of
      * indexes (i, j) where i, j < (array length), i < j, and arr(i) > arr(j).
      *
      * Approach: Divide and conquer (very similar to merge sort)
      *
      * Time complexity: O(n log n)
      *
      * @param arr
      * @tparam T
      */
    def countInversions[T <% Ordered[T]:ClassTag](arr: Array[T]) = {
        def countHelper(start: Int, end: Int): (Int, Array[T]) = {
            if(start + 1 == end) {
                (0, Array[T](arr(start)))
            }
            else {
                val mid = (start + end) / 2
                val (leftInversions, leftSorted) = countHelper(start, mid)
                val (rightInversions, rightSorted) = countHelper(mid, end)
                var i = 0
                var j = 0
                var crossInversions = 0
                val newArr: ArrayBuffer[T] = ArrayBuffer()
                while(i < leftSorted.length && j < rightSorted.length) {
                    if(leftSorted(i) <= rightSorted(j)) {
                        newArr append leftSorted(i)
                        i = i + 1
                    } else {
                        crossInversions = crossInversions + (leftSorted.length - i)
                        newArr append rightSorted(j)
                        j = j + 1
                    }
                }
                if(i < leftSorted.length) {
                    while(i < leftSorted.length) {
                        newArr append leftSorted(i)
                        i = i + 1
                    }
                } else {
                    while(j < rightSorted.length) {
                        newArr append rightSorted(j)
                        j = j + 1
                    }
                }
                (leftInversions + rightInversions + crossInversions, newArr.toArray[T])
            }
        }
        countHelper(0, arr.length - 1)._1
    }

}
