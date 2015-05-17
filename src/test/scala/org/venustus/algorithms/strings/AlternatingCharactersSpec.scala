package org.venustus.algorithms.strings

import org.scalatest.{Matchers, FlatSpec}

/**
 * Created by venkat on 13/05/15.
 */
class AlternatingCharactersSpec extends FlatSpec with Matchers {
    "AlternatingCharacters algorithm" should "remove consecutive repeated characters" in {
        AlternatingCharacters.removeRepeatingChars("AAAA") should be ("A")
        AlternatingCharacters.removeRepeatingChars("BBBBB") should be ("B")
        AlternatingCharacters.removeRepeatingChars("ABABABAB") should be ("ABABABAB")
        AlternatingCharacters.removeRepeatingChars("BABABA") should be ("BABABA")
        AlternatingCharacters.removeRepeatingChars("AAABBB") should be ("AB")
    }
}
