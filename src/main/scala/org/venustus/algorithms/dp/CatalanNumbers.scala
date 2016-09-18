package org.venustus.algorithms.dp

/**
 * Created by venkat on 08/01/15.
 */
object CatalanNumbers {

    /**
     * Computes nth catalan number.
     *
     * Catalan number is given by the recursive relation:
     *
     * C_0 = 1
     * C_n+1 = Sum over all 0 to n (c_i * c_n-i)
     *
     * Algorithm:
     *
     * The optimal sub structure and overlapping sub problems are obvious in this problem.
     * We maintain an array of integers of size n + 1 and iteratively compute larger and larger catalan numbers.
     *
     * Time complexity: O(n^2)
     *
     * @param n
     * @return
     */
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
