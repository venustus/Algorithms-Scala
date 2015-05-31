package org.venustus.algorithms.strings

/**
 * Created by venkat on 31/05/15.
 */
object PalindromeIndex {

    def isPalindrome(str: Array[Char], start: Int, end: Int): Boolean = {
        if(start >= end) true
        else if(str(start) == str(end)) isPalindrome(str, start + 1, end - 1)
        else false
    }

    /**
     * Given a string, find an index such that if we remove the character
     * at that index, the resulting string is a palindrome.
     * If the string is already a palindrome, return -1
     *
     * Assumptions: Either the string is a palindrome or there will always be such a character.
     *
     * The key insight for this problem is that we'll start looking from the edges and
     * if the characters on the edges match, then we can just ignore those two matched characters
     * and look at just the substring separately as it's own independent problem.
     *
     * If the edges does not match, then we have to replace one of them. Which one, depends on
     * the characters that are just inside. Compare first character with last but one and last with
     * second and whichever combination matches, we should delete the other one.
     *
     * If we reach the middle without finding such an index, it is already a palindrome.
     *
     * Time complexity: O(n)
     *
     * @param in
     */
    def findCharToBeRemovedForPalindrome(in: String) = {
        val inArr = in.toCharArray
        def helper(start: Int, end: Int): Int = {
            if(start == end) -1
            else if(start + 1 == end && inArr(start) == inArr(end)) -1
            else if(start + 1 == end && inArr(start) != inArr(end)) start
            else {
                // at least 3 characters are present in this case
                if (inArr(start) == inArr(end)) helper(start + 1, end - 1)
                else {
                    if(isPalindrome(inArr, start + 1, end)) start
                    else if(isPalindrome(inArr, start, end - 1)) end
                    else throw new IllegalArgumentException("Invalid input")
                }
            }
        }
        helper(0, in.size - 1)
    }

}
