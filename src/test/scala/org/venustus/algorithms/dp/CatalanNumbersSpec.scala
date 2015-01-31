package org.venustus.algorithms.dp

import org.scalatest.{Matchers, FlatSpec}

/**
 * Created by venkat on 08/01/15.
 */
class CatalanNumbersSpec extends FlatSpec with Matchers {
    "Catalan number generator" should "correct generate nth catalan number" in {
        CatalanNumbers getNthCatalanNumber(0) should be (1)
        CatalanNumbers getNthCatalanNumber(1) should be (1)
        CatalanNumbers getNthCatalanNumber(2) should be (2)
        CatalanNumbers getNthCatalanNumber(3) should be (5)
        CatalanNumbers getNthCatalanNumber(4) should be (14)
        CatalanNumbers getNthCatalanNumber(5) should be (42)
        CatalanNumbers getNthCatalanNumber(6) should be (132)
        CatalanNumbers getNthCatalanNumber(7) should be (429)
        CatalanNumbers getNthCatalanNumber(8) should be (1430)
        CatalanNumbers getNthCatalanNumber(9) should be (4862)
    }
}
