package org.venustus.algorithms.strings

import scala.util.control.Breaks._

/**
 * Created by venkat on 20/05/15.
 */
object SubstringDiff {

    /**
     * Given two strings of same length, find maximum value of l, such that M(i, j, l) <= s
     * where the function M is the diff count - number of mismatches between substrings of length l starting
     * from i and j respectively in str1 and str2.
     *
     * Bruteforce algorithm:
     *
     * Run 2 loops on string 1 and string 2, deciding starting points in each string
     * and for each such combination, iteratively try increasing amounts of l until we reach a breaking point
     * - where diff count exceeds s.
     *
     * Time complexity: O(n * n * n)
     *
     * @param str1
     * @param str2
     * @param s
     * @return
     */
    def findMaxSubstringWithLimitedDiffBruteForce(str1: String, str2: String, s: Int) = {
        if(str1.length != str2.length) throw new IllegalArgumentException("Two strings must be equal length")
        var maxL = 0
        for(i <- 0 until str1.length) {
            for(j <- 0 until str2.length) {
                var l = 0
                var diffCount = 0
                breakable {
                    while (i + l < str1.length && j + l < str2.length) {
                        if (str1.charAt(i + l) != str2.charAt(j + l)) diffCount = diffCount + 1
                        if (diffCount > s) break
                        l = l + 1
                    }
                }
                if(l > maxL) maxL = l
            }
        }
        maxL
    }

    /**
     * Given two strings of same length, find maximum value of l, such that M(i, j, l) <= s
     * where the function M is the diff count - number of mismatches between substrings of length l starting
     * from i and j respectively in str1 and str2.
     *
     * Algorithm:
     * The <code>getBest</code> sub-routine, given i and j retrieves longest value of l
     * for all i' and j' for which the diff count is under the limit specified, for the same |i - j|.
     * Since we are using sliding window algorithm inside the <code>getBest</code> sub routine,
     * it takes O(n) time.
     *
     * We only need to check for all possible values of i - j. There are at most 2n-1 distinct such combinations.
     * So, the overall complexity is O(n * n).
     *
     *
     * @param str1
     * @param str2
     * @param s
     * @return
     */
    def findMaxSubstringWithLimitedDiffEfficient(str1: String, str2: String, s: Int) = {
        /**
         * Returns maximum value of l among all (i', j') such that i' - i = j' - j
         * while M(i', j', l) <= k.
         *
         * This uses sliding window algorithm. The key observation is that if for some
         * i, j, l is maximum such that M(i, j, l) <= k then M(i + 1, j + 1, m) <=k for all m <= l - 1.
         * This is because, depending on whether str1(i) == str2(j) or not, difference for the string
         * starting from i + 1 and j + 1 is either k or k - 1. So, we don't need to compute any of
         * M(i + 1, j + 1, m) for any m <= l - 1 since at least l - 1 integers satisfied the property.
         *
         * Time complexity: O(n) - where n is the size of the string
         *
         * @param i
         * @param j
         */
        def getBest(i: Int, j: Int, bestTillNow: Int, lastL: Int, previousDiffCount: Int): Int = {
            var diffCount = previousDiffCount
            var maxL = 0
            var iP = i + lastL - 1
            var jP = j + lastL - 1
            println(s"getBest invoked with: i: $i, j: $j, bestTillNow: $bestTillNow, lastL: $lastL, previousDiffCount: $previousDiffCount")
            if(iP >= str1.length || jP >= str2.length) bestTillNow
            else {
                do {
                    println(s"Comparing $iP: ${str1.charAt(iP)} with $jP: ${str2.charAt(jP)}")
                    if (str1.charAt(iP) != str2.charAt(jP)) diffCount = diffCount + 1
                    iP = iP + 1
                    jP = jP + 1
                } while (iP < str1.length && jP < str2.length && diffCount <= s)
                println(s"diffCount after this iteration: $diffCount")
                println(s"Stopped at iP: $iP and jP: $jP")
                maxL = math.max(bestTillNow, iP - 1 - i)
                println(s"maxL string pair: ${str1.substring(i, iP - 1)}, ${str2.substring(j, jP - 1)}")
                val nextDiffCount = diffCount - (if(str1.charAt(iP - 1) != str2.charAt(jP - 1)) 1 else 0) - (if(str1.charAt(i) != str2.charAt(j)) 1 else 0)
                getBest(i + 1, j + 1, maxL, iP - 1 - i, nextDiffCount)
            }
        }
        var maxL = 0
        for(i <- 0 until str1.length) {
            maxL = math.max(maxL, math.max(getBest(0, i, 0, 1, 0), getBest(i, 0, 0, 1, 0)))
        }
        maxL
    }
}
