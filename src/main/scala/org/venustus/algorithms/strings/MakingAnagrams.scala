package org.venustus.algorithms.strings

import scala.collection.mutable.Map

/**
 * Created by venkat on 14/05/15.
 */
object MakingAnagrams {

    def strToCharMap(str: String) = {
        (Map[Char, Int]() /: str.toCharArray)((acc, ch) => { acc += (ch -> ((acc getOrElse (ch, 0)) + 1)); acc })
    }

    def getCumulativeSum(map: Map[Char, Int]) = {
        (0 /: map.keySet)((acc, key) => acc + map(key))
    }

    def getCumulativeDiff(map1: Map[Char, Int], map2: Map[Char, Int]) = {
        val commonKeys = map1.keySet & map2.keySet
        (0 /: commonKeys)((acc, ch) => acc + math.abs(map1(ch) - map2(ch)))
    }

    /**
     * Given two strings, returns minimum number of character deletions that are required to
     * make them anagrams.
     *
     * Basically we need to delete three kinds of characters -
     *     - characters that are in first string but not in second
     *     - characters that are in second string but not in first
     *     - characters that are in both strings, but counts are different
     *
     * Time complexity: O(n) where n is the sum of lengths of two strings
     *
     * @param str1
     * @param str2
     * @return
     */
    def makeAnagram(str1: String, str2: String) = {
        val map1 = strToCharMap(str1)
        val map2 = strToCharMap(str2)
        getCumulativeSum(map1 -- map2.keySet) + getCumulativeSum(map2 -- map1.keySet) + getCumulativeDiff(map1, map2)
    }

    /**
     * Given a string <code>str</code> which is a concatenation of two strings <code>str1</code> and <code>str2</code>
     * where |len(str1) - len(str2)| <= 1. Find minimum number of changes to characters to be made in the
     * first string <code>str1</code> to make it an anagram of <code>str2</code>.
     *
     * Basically answer consists of two parts - characters that are exclusively in either of the strings
     * can be converted straight away to the corresponding ones in the other. But that may not be enough. Even
     * among the common keys, there may be difference in counts and hence they may need to be converted.
     *
     * Time complexity: O(n) where n is length of the string
     * @param str
     * @return Some integer if two sub strings can be made anagrams of each other, None otherwise
     */
    def makeAnagram2(str: String): Option[Int] = {
        val (str1, str2) = str.splitAt(str.size / 2)
        // if both are not of equal length, we cannot make them anagrams of each other
        if(str1.size != str2.size) None
        else {
            val map1 = strToCharMap(str1)
            val map2 = strToCharMap(str2)
            // number of characters exclusively in first string
            val extraInFirst = getCumulativeSum(map1 -- map2.keySet)
            // number of characters exclusively in second string
            val extraInSecond = getCumulativeSum(map2 -- map1.keySet)
            val commonKeys = map1.keySet & map2.keySet
            val commonCharsExtraInFirst = (Map[Char, Int]() /: commonKeys)((acc, ch) => {
                if(map1(ch) == map2(ch)) acc
                else {
                    acc += (ch -> (map1(ch) - map2(ch)))
                    acc
                }
            })
            // case where they are already anagrams of each other
            if(extraInFirst == 0 && extraInSecond == 0 && commonCharsExtraInFirst.isEmpty) Some(0)
            else {
                // if they don't balance out each other, we can't make anagrams of each other
                if(extraInFirst - extraInSecond + commonCharsExtraInFirst.values.sum != 0) None
                else {
                    if(commonCharsExtraInFirst.isEmpty) {
                        // case where common characters matched exactly
                        // in this case, only exclusive characters need to be swapped
                        if(extraInFirst != extraInSecond) None
                        else Some(extraInFirst)
                    }
                    else {
                        if (extraInFirst >= extraInSecond) {
                            // there are more exclusive characters in first string than in second
                            // we'll convert all exclusive characters in first, but then
                            // we should also convert some of the characters whose count mismatches
                            Some(extraInFirst + commonCharsExtraInFirst.values.filter((n) => n > 0).sum)
                        }
                        else {
                            // there are more exclusive characters in second string than in first
                            // we'll convert to all exclusive characters in second, but then
                            // we should also convert some of the characters whose count mismatches
                            Some(extraInSecond + commonCharsExtraInFirst.values.filter((n) => n < 0).map((n) => math.abs(n)).sum)
                        }
                    }
                }
            }
        }
    }
}
