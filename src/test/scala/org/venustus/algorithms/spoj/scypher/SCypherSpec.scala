package org.venustus.algorithms.spoj.scypher

import org.scalatest.{FlatSpec, Matchers}

/**
 * Created by venkat on 26/01/15.
 */
class SCypherSpec extends FlatSpec with Matchers {

    "Char pair differentiation" should "happen successfully" in {
        SCypher.getDifferentiatingCharPair("cebdbac", "cac") should be (Some('e', 'a'))
        SCypher.getDifferentiatingCharPair("cac", "ecd") should be (Some('c', 'e'))
        SCypher.getDifferentiatingCharPair("aba", "bac") should be (Some('a', 'b'))
        SCypher.getDifferentiatingCharPair("aacb", "aac") should be (None)
    }

    "A simple substitution cipher" should "break if total ordering can be obtained from encrypted words" in {
        SCypher.retrieveCipherKey(5, Vector("cebdbac", "cac", "ecd", "dca", "aba", "bac")) should be (Some(Vector('c', 'e', 'd', 'a', 'b')))
        //SCypher.retrieveCipherKey2(5, Vector("cebdbac", "cac", "ecd", "dca", "aba", "bac")) should be (Some(Vector('c', 'e', 'd', 'a', 'b')))
        SCypher.decryptSubstitutionCipher(5, Vector("cebdbac", "cac", "ecd", "dca", "aba", "bac"), "cedab", SCypher.retrieveCipherKey) should be ("abcde")
        //SCypher.decryptSubstitutionCipher(5, Vector("cebdbac", "cac", "ecd", "dca", "aba", "bac"), "cedab", SCypher.retrieveCipherKey2) should be ("abcde")

        SCypher.retrieveCipherKey(4, Vector("cca", "cad", "aac", "bca", "dab")) should be (Some(Vector('c', 'a', 'b', 'd')))
        //SCypher.retrieveCipherKey2(4, Vector("cca", "cad", "aac", "bca", "dab")) should be (Some(Vector('c', 'a', 'b', 'd')))
        SCypher.decryptSubstitutionCipher(4, Vector("cca", "cad", "aac", "bca", "dab"), "bdac", SCypher.retrieveCipherKey) should be ("cdba")
        //SCypher.decryptSubstitutionCipher(4, Vector("cca", "cad", "aac", "bca", "dab"), "bdac", SCypher.retrieveCipherKey2) should be ("cdba")

        SCypher.retrieveCipherKey(7, Vector("deg", "dg", "e", "c", "bf", "bad", "fd", "fdg", "fdc")) should be (Some(Vector('d', 'e', 'g', 'c', 'b', 'f', 'a')))
        //SCypher.retrieveCipherKey2(7, Vector("deg", "dg", "e", "c", "bf", "bad", "fd", "fdg", "fdc")) should be (Some(Vector('d', 'e', 'g', 'c', 'b', 'f', 'a')))
    }

    it should " not break if total ordering cannot be obtained from encrypted words" in {
        SCypher.retrieveCipherKey(4, Vector("cca", "cad", "aac", "bca")) should be (None)
        //SCypher.retrieveCipherKey2(4, Vector("cca", "cad", "aac", "bca")) should be (None)

        SCypher.decryptSubstitutionCipher(4, Vector("cca", "cad", "aac", "bca"), "bdac", SCypher.retrieveCipherKey) should be ("Message cannot be decrypted.")
        //SCypher.decryptSubstitutionCipher(4, Vector("cca", "cad", "aac", "bca"), "bdac", SCypher.retrieveCipherKey2) should be ("Message cannot be decrypted.")
    }

}
