package org.venustus.algorithms.dp

import org.scalatest.{FlatSpec, Matchers}

/**
 * Created by venkat on 11/01/15.
 */
class WordBreakSpec extends FlatSpec with Matchers {
    "Work break algorithm" should "correctly recognize breakable sentences for trivial examples" in {
        val dict = Map(List("i", "like", "sam", "sung", "samsung", "mobile", "ice", "cream", "icecream", "man", "go", "mango").zip(List.fill(12) { true }) : _*)

        WordBreak.breakSentenceIntoWords("ilikesamsung", dict) should be (true)
        WordBreak.breakSentenceIntoWords("ilikeicecream", dict) should be (true)
        WordBreak.breakSentenceIntoWords("ilikeicesam", dict) should be (true)
        WordBreak.breakSentenceIntoWords("ilikesams", dict) should be (false)
    }
}
