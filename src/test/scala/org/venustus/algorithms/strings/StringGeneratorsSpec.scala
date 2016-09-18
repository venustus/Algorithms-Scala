package org.venustus.algorithms.strings

import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by venkat on 17/09/16.
  */
class StringGeneratorsSpec extends FlatSpec with Matchers {

    "String generator" should "generate all permutations properly" in {
        (StringGenerators generatePermutation "abc") should be (
            Set("abc", "acb", "bac", "bca", "cba", "cab")
        )
        (StringGenerators generatePermutation "aaa") should be (
            Set("aaa", "aaa", "aaa", "aaa", "aaa", "aaa")
        )
        (StringGenerators generatePermutation "abcd") should be (
            Set("abcd", "abdc", "acbd", "acdb", "adcb", "adbc", "bacd", "badc", "bcad", "bcda", "bdac", "bdca",
                "cabd", "cadb", "cdab", "cdba", "cbad", "cbda", "dabc", "dacb", "dbca", "dbac", "dcab", "dcba")
        )
        (StringGenerators generatePermutation "abcdefghij").size should be (10 * 9 * 8 * 7 * 6 * 5 * 4 * 3 * 2 * 1)
    }

    it should "generate all combinations properly" in {
        (StringGenerators generateCombinations "wxyz") should be (
            Set("wxyz", "w", "x", "y", "z", "wx", "wy", "wz", "xy", "xz", "yz", "wxy", "wxz", "wyz", "xyz")
        )
        (StringGenerators generateCombinations "aab") should be (
            Set("aab", "a", "b", "ab", "aa")
        )
    }

    "Phone number to string converter" should "print aliases of phone numbers correctly" in {
        StringGenerators printWordsForPhoneNumber Array(4, 9, 7, 1, 9, 2, 7)
    }

}
