package org.venustus.algorithms.dp

/**
 * Created by venkat on 26/07/14.
 */
object OptimalBST {

    def computeOptimalBST(frequencies: Vector[Double]): Double = {
        val n = frequencies.size
        val finalRes = (0 until n).foldLeft(Vector.fill(n, n)(0.0))((result, s) => {
            (0 until n).foldLeft(result)((res, i) => {
                if(i + s < n) {
                    println("Computing value result(" + i + ", " + (i + s) + ")")
                    val term1 = (i to i + s).foldLeft(0.0)(_ + frequencies(_))
                    val term2 = (for(r <- i to i + s) yield {println("r = " + r); ((if(i <= (r - 1)) {println(res(i)(r - 1)); res(i)(r - 1)} else {println(0.0); 0.0}) + (if((r + 1) <= (i + s)) {println(res(r + 1)(i + s)); res(r + 1)(i + s)} else {println(0.0); 0.0}))}).min
                    println("term1: " + term1)
                    println("term2: " + term2)
                    println("Updating res(" + i + ")(" + (i + s) + ") with " + (term1 + term2))
                    res.updated(i, res(i).updated(i + s, term1 + term2))
                }
                else res
            })
        })
        finalRes(0)(n - 1)
    }
}
