package org.venustus.algorithms.strings

/**
 * Created by venkat on 13/05/15.
 */
object AlternatingCharacters {
    /**
     * Given a string of characters with repetitions, remove all repeating and consecutive
     * characters and return the resulting string.
     *
     * A naive algorithm for this would run through the string, comparing consecutive characters
     * and removing characters in each iteration. But because String is immutable, the only way to
     * do this is to construct a new string in each iteration using a sub string of the previous string.
     * In JDK7, the implementation of <code>substring</code> changed to O(n) from being O(1) in JDK6.
     * Hence the naive algorithm would end up with time complexity of O(n^2).
     *
     * Using divide and conquer paradigm, however, we can arrive at an efficient algorithm like this:
     *
     * 1) Divide the string into two (almost) equal halves.
     * 2) Remove repeating characters in each of the halves, recursively.
     * 3) Combine the two answers into a new answer - we only have to take care of one case here - if the
     *    last character of the first answer is same as first character of the second one.
     *
     * At each step, we divide into 2 halves each of size n / 2. The work outside recursive calls is O(n)
     * because it contains calls to <code>slice</code> which internally uses <code>substring</code>.
     *
     * So, this falls under case 2 of master theorem, and hence overall time complexity is Theta(n log n).
     *
     * @param in
     * @return
     */
    def removeRepeatingChars(in: String): String = {
        in.length match {
            case 0 | 1 => in
            case 2 => if(in.charAt(0) == in.charAt(1)) String.valueOf(in.charAt(0)) else in
            case _ => {
                val split = in.length / 2
                val ans1 = removeRepeatingChars(in.slice(0, split))
                val ans2 = removeRepeatingChars(in.slice(split, in.length))
                if(ans1.charAt(ans1.length - 1) == ans2.charAt(0)) ans1 + ans2.slice(1, ans2.length)
                else ans1 + ans2
            }
        }
    }
}
