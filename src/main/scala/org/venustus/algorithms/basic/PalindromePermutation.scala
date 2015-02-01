package org.venustus.algorithms.basic

/**
 * Created by venkat on 01/02/15.
 */
object PalindromePermutation {
    /**
     * Given a string s, returns true if and only if
     * there exists a permutation of s which is a palindrome.
     *
     * Algorithm:
     *
     * 1) First loop through the characters in the string and count the letters by keeping in a map
     * 2) Next extract values from the map and count the number of odd numbers in the list of values.
     * 3) If the number of odd numbers is greater than 1, return false otherwise return true
     *
     * Time complexity: O(n)
     * Space complexity: O(n)
     * @param s
     * @return
     */
    def palindromePermutationExists(s: String): Boolean = {
        val letterCounts = (Map[Char, Int]() /: s)((acc, ch) =>
            if(acc contains ch) acc updated (ch, acc(ch) + 1) else acc updated (ch, 1))
        letterCounts.values.count(_ % 2 != 0) <= 1
    }
}
