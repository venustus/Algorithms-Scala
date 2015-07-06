package org.venustus.algorithms.search

import scala.collection.immutable.Stack

/**
 * Created by venkat on 02/06/15.
 */
object IndexProduct {

    /**
     * Given an integer array as input, find maximum index product among all indexes.
     * Index product for an index i is defined as LEFT(i) * RIGHT(i) where
     * LEFT(i) is the nearest index to the left of i such that arr(LEFT(i)) > arr(i)
     * and RIGHT(i) is the nearest index to the right of i such that arr(RIGHT(i)) > arr(i).
     *
     * Algorithm:
     *
     * The brute force algorithm for this is of O(n ** 2).
     *
     * However, we can significantly speed up the algorithm using the following observation:
     *
     * For any given index i, the LEFT(i) can be either the immediately to the left of i, if arr(i - 1) > arr(i)
     * or we can use LEFT(i - 1) to skip many elements for comparison. If arr(i - 1) < arr(i), then LEFT(i) is definitely
     * <= LEFT(i - 1). So, we start comparing arr(LEFT(i - 1)) and so on.
     *
     * Although for a given index, this can take O(n), in an amortized analysis, it turns out that overall
     * time complexity is O(n).
     *
     * @param arr
     * @return
     */
    def findMaxIndexProduct(arr: Array[Long]): Long = {
        val leftRes = (Array.fill[Int](arr.size)(0) /: (1 until arr.size))((acc, n) => {
             def isGreaterToLeft(li: Int): Int = {
                 if(li < 0) 0
                 else if(arr(li) > arr(n)) li + 1
                 else isGreaterToLeft(acc(li) - 1)
             }
            acc(n) = isGreaterToLeft(n - 1)
            acc
        })
        val rightRes = (Array.fill[Int](arr.size)(0) /: Range(arr.size - 2, -1, -1))((acc, n) => {
            def isGreaterToRight(li: Int): Int = {
                if(li >= arr.size || li < 0) 0
                else if(arr(li) > arr(n)) li + 1
                else isGreaterToRight(acc(li) - 1)
            }
            acc(n) = isGreaterToRight(n + 1)
            acc
        })
        (0L /: (leftRes zip rightRes))((acc, index) => {
            index match {
                case (left, right) => math.max(acc, left.toLong * right.toLong)
            }
        })
    }

}
