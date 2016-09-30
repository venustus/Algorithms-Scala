package org.venustus.algorithms.sorting

import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by venkat on 18/09/16.
  */
class InversionCounterSpec extends FlatSpec with Matchers {

    "Inversion counter" should "count inversions" in {
        InversionCounter countInversions Array(4, 3, 1, 2, 8, 6, 10) should be (6)
    }

}
