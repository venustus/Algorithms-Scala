package org.venustus.algorithms.miscellaneous

/**
 * Created by venkat on 03/05/15.
 */
object KaprekarNumbers {

    def numDigits(n: Long): Int = {
        if(n == 0) 0
        else (numDigits(n / 10) + 1)
    }

    def isKaprekarNumber(n: Long) = {
        val d = numDigits(n)
        val square = n * n
        val l = (square /: (1 to d))((acc, i) => acc / 10)
        val rMultiplier = (1 /: (1 to d))((acc, i) => acc * 10)
        val r = square - (l * rMultiplier)
        l + r == n
    }

    def getKaprekarNumbersInRange(low: Long, high: Long) = {
        (Vector[Long]() /: (low to high))((acc, n) => {
            if(isKaprekarNumber(n)) acc :+ n
            else acc
        })
    }
}
