package org.venustus.algorithms.sorting

/**
 * Created by venkat on 05/06/15.
 */
object AlmostSorted {

    def isAlmostSorted(arr: Array[Int]): Option[String] = {
        val (_, inversions) = ((0, Vector[Int]()) /: arr.zipWithIndex)((acc, ith) => {
            ith match {
                case (num, i) => {
                    acc match {
                        case (prev, inversions) => {
                            if(prev > num) (num, inversions :+ i)
                            else (num, inversions)
                        }
                    }
                }
            }
        })
        if(inversions.size == 0) Some("yes")
        else if(inversions.size == 1) {
            if(inversions(0) == arr.size - 1 || arr(inversions(0) - 1) < arr(inversions(0) + 1)) {
                Some("swap " + inversions(0) + " " + (inversions(0) + 1))
            }
            else None
        }
        else if(inversions.size == 2) {
            Some("swap " + inversions(0) + " " + (inversions(1) + 1))
        }
        else {
            val isPossibleByReversing = ((-1, true) /: inversions)((acc, inv) => {
                acc match {
                    case (prevIndex, status) => (inv, status && (prevIndex == -1 || inv == prevIndex + 1))
                }
            })
            if(isPossibleByReversing._2) Some("reverse " + inversions(0) + " " + (inversions(inversions.size - 1) + 1))
            else None
        }
    }

}
