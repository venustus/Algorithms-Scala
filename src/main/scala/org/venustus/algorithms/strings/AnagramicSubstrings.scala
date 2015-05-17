package org.venustus.algorithms.strings

import scala.collection.mutable.Map
/**
 * Created by venkat on 17/05/15.
 */
object AnagramicSubstrings {

    def strToCharMap(str: String) = {
        (Map[Char, Int]() /: str.toCharArray)((acc, ch) => { acc += (ch -> ((acc getOrElse (ch, 0)) + 1)); acc })
    }

    def areAnagrams(str1: String, str2: String) = {
        val map1 = strToCharMap(str1)
        val map2 = strToCharMap(str2)
        map1.equals(map2)
    }

    def getSubstringsOfLength(str: String, l: Int) =
        for {
            i <- 0 until (str.length + 1 - l)
            j = i + l
        } yield str.slice(i, j)

    def findNumberOfAnagramicSubstrings(str: String) = {
        var count = 0
        for(l <- 1 to str.length) {
            val substrings = getSubstringsOfLength(str, l)
            val allPairs = for {
                i <- 0 until (substrings.size - 1)
                j <- i + 1 until substrings.size
            } yield (substrings(i), substrings(j))
            count += (0 /: allPairs)((acc, pr) => {
                pr match {
                    case (str1, str2) => if(areAnagrams(str1, str2)) acc + 1 else acc
                }
            })
        }
        count
    }

}
