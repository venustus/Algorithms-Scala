package org.venustus.algorithms.strings

/**
 * Created by venkat on 03/05/15.
 */
object Successor {

    /**
     * Given a word (all lower case without spaces or punctuation), finds the next lexicographically
     * bigger permutation of the same word. If no such permutation exists, then returns <code>None</code>.
     *
     * The main observation is that as long as characters are in decreasing order from the start of the string,
     * we can't find the next string. We need to manipulate the string only from the point, the characters
     * start increasing. The first such character should be swapped with the minimum among the subsequent character
     * and then all the subsequent characters should be sorted (to get the immediately next string).
     *
     * Time complexity: O(n) where n is the size of the string
     * @param str
     * @return
     */
    def findNextBiggerWord(str: String): Option[String] = {
        val chars = str.toCharArray
        var i = chars.size - 1
        while(i > 0 && chars(i).toInt <= chars(i - 1).toInt) i = i - 1
        if(i == 0) None
        else {
            val minCharIndex = i + chars.slice(i, chars.size).zipWithIndex.filter{ case (ch, chIndex) => ch.toInt > chars(i - 1).toInt }.min._2
            val tmp = chars(i - 1)
            chars(i - 1) = chars(minCharIndex)
            chars(minCharIndex) = tmp
            Some((chars.slice(0, i) ++ chars.slice(i, chars.size).sorted).mkString)
        }
    }
}
