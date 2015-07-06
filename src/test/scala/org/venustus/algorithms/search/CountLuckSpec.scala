package org.venustus.algorithms.search

import org.scalatest.{Matchers, FlatSpec}

/**
 * Created by venkat on 03/06/15.
 */
class CountLuckSpec extends FlatSpec with Matchers {

    "Hermione Granger" should "find exit from forbidden forest" in {
        CountLuck.isHermioneSaved(Array(".X.X......X".toCharArray, ".X*.X.XXX.X".toCharArray,
            ".XX.X.XM...".toCharArray, "......XXXX.".toCharArray), 3) should be (true)
        CountLuck.isHermioneSaved(Array(".X.X......X".toCharArray, ".X*.X.XXX.X".toCharArray,
            ".XX.X.XM...".toCharArray, "......XXXX.".toCharArray), 4) should be (false)

        CountLuck.isHermioneSaved(Array("*.M".toCharArray, ".X.".toCharArray), 1) should be (true)
        CountLuck.isHermioneSaved(Array("*.M".toCharArray, ".X.".toCharArray), 0) should be (false)
    }

}
