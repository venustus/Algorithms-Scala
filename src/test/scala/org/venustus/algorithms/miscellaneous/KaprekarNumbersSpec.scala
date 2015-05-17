package org.venustus.algorithms.miscellaneous

import org.scalatest.{Matchers, FlatSpec}

/**
 * Created by venkat on 03/05/15.
 */
class KaprekarNumbersSpec extends FlatSpec with Matchers {

    "numDigits function" should "return correct number of digits" in {
        KaprekarNumbers.numDigits(2025) should be (4)
        KaprekarNumbers.numDigits(1) should be (1)
        KaprekarNumbers.numDigits(56) should be (2)
        KaprekarNumbers.numDigits(123123) should be (6)
    }

    "isKaprekarNumber function" should "tell correctly whether a given number is kaprekar number" in {
        KaprekarNumbers.isKaprekarNumber(45) should be (true)
        KaprekarNumbers.isKaprekarNumber(99) should be (true)
        KaprekarNumbers.isKaprekarNumber(76) should be (false)
    }

}
