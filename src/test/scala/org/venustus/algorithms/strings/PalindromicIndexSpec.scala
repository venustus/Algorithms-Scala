package org.venustus.algorithms.strings

import org.scalatest.{FlatSpec, Matchers}

/**
 * Created by venkat on 31/05/15.
 */
class PalindromicIndexSpec extends FlatSpec with Matchers {

    "Function for finding palindromic index" should "return correct index" in {
        PalindromeIndex.findCharToBeRemovedForPalindrome("aaab") should be (3)
        PalindromeIndex.findCharToBeRemovedForPalindrome("bcbc") should (be (0) or be (3))
        PalindromeIndex.findCharToBeRemovedForPalindrome("abczba") should (be (2) or be (3))
        PalindromeIndex.findCharToBeRemovedForPalindrome("baa") should be (0)
        PalindromeIndex.findCharToBeRemovedForPalindrome("aaa") should be (-1)
        PalindromeIndex.findCharToBeRemovedForPalindrome("hgygsvlfcwnswtuhmyaljkqlqjjqlqkjlaymhutwsnwcwflvsgygh") should be (44)
    }

}
