package org.venustus.algorithms.strings

/**
 * Created by venkat on 13/07/15.
 */
object ValidString {

    def strToCharMap(str: String) = {
        (Map[Char, Int]() /: str.toCharArray)((acc, ch) => { acc updated (ch, ((acc getOrElse (ch, 0)) + 1)) })
    }

    def charMapToCountMap(charMap: Map[Char, Int]) = {
        (Map[Int, Set[Char]]() /: charMap.keySet)((acc, charMapKey) => {
            acc updated (charMap(charMapKey), (acc getOrElse (charMap(charMapKey), Set[Char]())) + charMapKey)
        })
    }

    def isValidString(s: String) = {
        val charMap = strToCharMap(s)
        val countMap = charMapToCountMap(charMap)
        println(countMap)
        val countMapMinKey = countMap.keySet.min
        val countMapMaxKey = countMap.keySet.max
        (countMap.size == 1 ||
            (countMap.size == 2 &&
                ((countMapMaxKey - countMapMinKey == 1 &&
                countMap(countMapMaxKey).size == 1) ||
                    (countMapMinKey == 1 && countMap(countMapMinKey).size == 1))))
    }

}
