package org.venustus.algorithms.strings

import org.scalatest.{Matchers, FlatSpec}

/**
 * Created by venkat on 17/05/15.
 */
class AnagramicSubstringsSpec extends FlatSpec with Matchers {

    "Anagramic substrings counter" should "count anagramic substrings correctly" in {
        AnagramicSubstrings.findNumberOfAnagramicSubstrings("abba") should be (4)
        AnagramicSubstrings.findNumberOfAnagramicSubstrings("abcd") should be (0)
        AnagramicSubstrings.findNumberOfAnagramicSubstrings("ifailuhkqq") should be (3)
        AnagramicSubstrings.findNumberOfAnagramicSubstrings("hucpoltgty") should be (2)
        AnagramicSubstrings.findNumberOfAnagramicSubstrings("ovarjsnrbf") should be (2)
        AnagramicSubstrings.findNumberOfAnagramicSubstrings("pvmupwjjjf") should be (6)
        AnagramicSubstrings.findNumberOfAnagramicSubstrings("iwwhrlkpek") should be (3)
        AnagramicSubstrings.findNumberOfAnagramicSubstrings("ifailuhkqqhucpoltgtyovarjsnrbfpvmupwjjjfiwwhrlkpekxxnebfrwibylcvkfealgonjkzwlyfhhkefuvgndgdnbelgruel") should be (399)
    }

}
