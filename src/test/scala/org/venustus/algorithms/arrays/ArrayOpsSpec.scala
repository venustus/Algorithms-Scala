package org.venustus.algorithms.arrays

import org.scalatest.{FlatSpec, Matchers}
import scala.util.Random

/**
 * Created by venkat on 30/08/14.
 */
class ArrayOpsSpec extends FlatSpec with Matchers{
    "The in-place partition function" should "partition an array of numbers correctly" in {
        val arr: Array[Int] = Array(10, -4, 35, -28, -34, -33, -42, -35, 40, 26, 1)
        ArrayOps.partition(arr, (_ < 0))
        arr should be (Array(-4, -28, -34, -33, -42, -35, 10, 35, 40, 26, 1))
    }

    it should "partition a large array of randomly generated numbers correctly and quickly" in {
        val input = (Seq.fill(11)(Random.nextInt(1000) - 500)).toArray
        val (arr1, arr2) = input.partition((_ < 0))
        ArrayOps.partition(input, (_ < 0))
        (arr1 ++ arr2) should be (input)
    }

    "The function for finding max profit" should "correctly find the maximum profit for arrays of length 0, 1 and 2" in {
        ArrayOps.findMaxProfit(List()) should be (-1, -1)
        ArrayOps.findMaxProfit(List(5)) should be (0, 0)
        ArrayOps.findMaxProfit(List(5, 7)) should be (0, 1)
        ArrayOps.findMaxProfit(List(7, 6)) should be (0, 0)
    }

    it should "correctly find maxmimum profit for sample larger arrays" in {
        ArrayOps.findMaxProfit(List(32, 50, 28, 70, 45, 38, 86, 90, 58, 74)) should be (2, 7)
    }

    "The function for finding largest sum contiguous sub array" should "correctly find the correct sub array for small arrays" in {
        ArrayOps.getLargestSumContiguousSubsequence(List()) should be (-1, -1, Int.MinValue)
        ArrayOps.getLargestSumContiguousSubsequence(List(10)) should be(0, 0, 10)
        ArrayOps.getLargestSumContiguousSubsequence(List(10, 11)) should be (0, 1, 21)
        ArrayOps.getLargestSumContiguousSubsequence(List(10, -10)) should be (0, 0, 10)
        ArrayOps.getLargestSumContiguousSubsequence(List(-10, 10)) should be (1, 1, 10)
    }

    it should "correctly find largest sum sub array for sample larger arrays" in {
        ArrayOps.getLargestSumContiguousSubsequence(List(10, 20, -5, 6, 8, 4, 9, -4, -3, 12, -2)) should be (0, 9, 57)
    }
}
