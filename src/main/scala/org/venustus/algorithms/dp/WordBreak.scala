package org.venustus.algorithms.dp

/**
 * Created by venkat on 11/01/15.
 */
object WordBreak {

    /**
     * Given a sentence without spaces and a dictionary of words,
     * return true if and only if the sentence can be split into words
     * separated by spaces where each word is in dictionary.
     *
     * A crude recursive algorithm (which is exponential):
     *
     * 1) Try all prefixes of the sentence starting at index 0.
     * 2) For each prefix length i, first check if the prefix is in dictionary.
     *    If so, then recursively check if sentence(i + 1 .. n - 1)
     *    is breakable into meaningful words.
     *    If so, then return true, else check next prefix.
     *
     * A slick dynamic programming algorithm which stores the results for later use.
     *
     * 1) Initialize an array wb[] where wb[i] is true iff sentence[i .. n - 1] is breakable
     *    into meaningful words. Initially all elements in this array are false.
     * 2) Answer is in wb[0] at the end of the algorithm.
     * 3) wb[i] = for all j in (i + 1, n - 1) if dict contains sentence[i, j] and wb[j + 1] is true then set wb[i] = true and break
     * 4) return wb[0]
     *
     * Time complexity: O(n^2) where n is the size of the string
     * Space complexity: O(n)
     * @param sentence
     * @param dict
     */
    def breakSentenceIntoWords(sentence: String, dict: Map[String, Boolean]) = {
        val n = sentence.size
        val ans = Vector.fill(n + 1) { false } updated (n, true)
        val finalAns = ((ans updated (n - 1, dict contains sentence.substring(n - 1, n))) /: Range(n - 2, -1, -1)) ((acc, i) => {
            (acc /: Range(i + 1, n + 1)) ((acc2, j) => {
                acc2 updated (i, acc2(i) || ((dict contains sentence.substring(i, j)) && acc2(j)))
            })
        })
        finalAns(0)
    }

}
