package org.venustus.algorithms.search

import org.scalatest.{FlatSpec, Matchers}

/**
 * Created by venkat on 02/06/15.
 */
class IndexProductSpec extends FlatSpec with Matchers {
    "Algorithm for index product" should "return current index product" in {
        IndexProduct.findMaxIndexProduct(Array(5, 4, 3, 4, 5)) should be (8)
        IndexProduct.findMaxIndexProduct(Array(1, 2, 3, 4, 5)) should be (0)
        IndexProduct.findMaxIndexProduct(Array(5, 4, 3, 2, 1)) should be (0)
        IndexProduct.findMaxIndexProduct(Array(17, 1, 2, 3, 4, 5, 4, 3, 2, 1, 17)) should be (99)
    }

    "Algorithm for index product" should "work for very big arrays" in {
        val src = scala.io.Source.fromURL("file:///Users/venkat/Documents/Projects/Algorithms/src/test/resources/org/venustus/algorithms/search/100000.txt")
        IndexProduct.findMaxIndexProduct(src.getLines.toList.head.split(" ").map(_.toLong)) should be (2500100000L)
    }
}
