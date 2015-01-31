package org.venustus.algorithms.dp

/**
 * Created by venkat on 08/01/15.
 */
object CatalanNumbers {

    def getNthCatalanNumber(n: Int) = {
        if(n == 0 || n == 1) 1
        else {
            val answer = ((List.fill(n + 1) { 0 } updated(0, 1)) /: (1 to n))((acc, i) => {
                acc updated(i, (acc(i) /: (0 until i)) { (acc2, j) => acc2 + acc(j) * acc(i - j - 1) })
            })
            answer(n)
        }
    }

}
