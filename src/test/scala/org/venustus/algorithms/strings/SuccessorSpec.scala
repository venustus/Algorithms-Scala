package org.venustus.algorithms.strings

import org.scalatest.{Matchers, FlatSpec}

/**
 * Created by venkat on 03/05/15.
 */
class SuccessorSpec extends FlatSpec with Matchers {
    "successor function" should "handle edge cases correctly" in {
        Successor.findNextBiggerWord("bb") should be (None)
        Successor.findNextBiggerWord("khdc") should be (None)
    }

    "successor function" should "find correct successor" in {
        Successor.findNextBiggerWord("ab") should be (Some("ba"))
        Successor.findNextBiggerWord("hefg") should be (Some("hegf"))
        Successor.findNextBiggerWord("dhck") should be (Some("dhkc"))
        Successor.findNextBiggerWord("dakc") should be (Some("dcak"))
        Successor.findNextBiggerWord("dkhc") should be (Some("hcdk"))
        Successor.findNextBiggerWord("abcfzyonlg") should be (Some("abcgflnoyz"))
    }
}
