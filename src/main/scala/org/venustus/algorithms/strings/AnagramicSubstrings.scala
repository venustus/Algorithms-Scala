package org.venustus.algorithms.strings

import scala.collection.mutable.Map
/**
 * Created by venkat on 17/05/15.
 */
object AnagramicSubstrings {

    /**
     * Returns a unique hash code for the character map of a string.
     * This is useful to identify anagrams since in angrams, individual character counts
     * matter.
     *
     * Time complexity: O(n) - where n is the size of the string
     *
     * @param str
     * @param start
     * @param end
     * @return
     */
    def strToCharMapHash(str: String, start: Int, end: Int) = {
        val charMap = (Map[Char, Int]() /: (start until end))((acc, chi) => {
            acc += (str.charAt(chi) -> ((acc getOrElse (str.charAt(chi), 0)) + 1))
            acc
        })
        charMap.hashCode
    }

    /**
     * Given a string, returns unique hash code values representing unique
     * character maps of all sub strings of length l.
     *
     * Basically, any duplicates in the returned list, indicate presence of anagrams.
     * @param str
     * @param l
     * @return
     */
    def getSubstringHashesOfLength(str: String, l: Int) =
        for {
            i <- 0 until (str.length + 1 - l)
            j = i + l
        } yield strToCharMapHash(str, i, j)

    /**
     * Computes and returns number of anagramic pairs of substrings of a given string - that is
     * number of pairs of substrings such that they are anagrams of each other.
     *
     * Algorithm:
     *
     * 1) For each substring of length l, compute a unique hash code representing the
     *    character map of that sub string.
     * 2) In one single loop, compute counts of each unique hash value.
     * 3) If a particular character map is repeated n times, then we can produce n choose 2 pairs
     *    of anagrams. Increment the count accordingly.
     *
     * Time complexity: O(n ** 2)
     * NOTE: Although at first look this is an O(n ** 3) solution, you must note that
     * there are only O(n ** 2) sub strings in total - n of them of size 1, n - 1 of them of size 2 and so on.
     * So, the two for loops are really just looping over all these sub strings and for each such sub string
     * we are computing a hash value and ultimately counting number of such unique hashes.
     *
     * @param str
     * @return
     */
    def findNumberOfAnagramicSubstrings(str: String) = {
        var count = 0
        for(l <- 1 to str.length) {
            val substringHashes = getSubstringHashesOfLength(str, l)
            val substringHashCounts: Map[Int, Int] = (Map[Int, Int]() /: substringHashes)((acc, hash) => {
                acc += (hash -> (acc.getOrElse(hash, 0) + 1))
                acc
            })
            count += (0 /: substringHashCounts.keySet)((acc, hash) => {
                val n: Int = substringHashCounts.getOrElse(hash, 0)
                acc + ((n * (n - 1)) / 2)
            })
        }
        count
    }

}
