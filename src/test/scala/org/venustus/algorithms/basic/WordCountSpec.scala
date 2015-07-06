package org.venustus.algorithms.basic

import org.scalatest.{Matchers, FlatSpec}

/**
 * Created by venkat on 30/08/14.
 */
class WordCountSpec extends FlatSpec with Matchers {
    "Word counter" should "count words properly" in {
        val count = WordCounter.countWords("/Users/venkat/Documents/Projects/Algorithms/src/test/resources/org/venustus/algorithms/basic/hacker.txt")
        println(count)
        count("is") should be (5)
    }
}
