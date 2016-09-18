package org.venustus.algorithms.dp

import org.venustus.algorithms.greedy.ClusterFinder
import scala.collection.immutable.HashMap

/**
 * Created by venkat on 27/07/14.
 */
object Knapsack {

    type Item = Pair[Int, Int]

    /**
     * Given a knapsack of capacity <code>knapsackSize</code> and a list of items
     * in which each item is represented by a pair of value and weight of each item,
     * compute the maximum value that can be obtained by selecting items under the constraint
     * that the total weight must be less than the capacity of knapsack.
     *
     * Approach: Dynamic Programming
     *
     * This one starts out with a 2-dimensional array - one dimension for the items available and one for
     * the capacity of the knapsack. Initially, all elements in this array are zeros.
     *
     * Optimal sub-structure:
     * The optimal sub-structure can be observed by observing the formula:
     *
     * OptimalValue(i, w) = max(OptimalValue(i - 1, w), Value(i) + OptimalValue(i - 1, w - Weight(i))
     *
     * The following code iteratively fills this two-dimensional array from the beginning.
     *
     * Time complexity: O(n * W) where n is the number of items and W is the total weight.
     *
     * Note that this problem is actually NP-complete because W is exponential in the size of input -
     * number of bits needed to represent W.
     * @param knapsackSize
     * @param items
     * @return
     */
    def computeOptimalSolution(knapsackSize: Int, items: Vector[Item]): Int = {
        val finalResult = (Vector.fill(items.size + 1, knapsackSize + 1)(0) /: (1 to items.size))((result, i) =>{
            (result /: (1 to knapsackSize))((res, w) => {
                res updated (i,
                    res(i) updated(w,
                        math.max(if(w < items(i - 1)._2) 0 else (items(i - 1)._1 + res(i - 1)(w - items(i - 1)._2)),
                                 res(i - 1)(w))))
            })
        })
        finalResult(items.size)(knapsackSize)
    }

    /**
     * This one computes exactly the same value as the previous function, but does not use a full two dimensional array
     * for storing the results. Instead, it uses plain old memoization technique and uses an optimized storage
     * so that the solution can work for very large instances.
     * @param knapsackSize
     * @param items
     * @return
     */
    def computeOptimalSolutionRecursive(knapsackSize: Int, items: List[Item]): Int = {
        val optimals: Map[Pair[Int, Int], Int] = new HashMap[Pair[Int, Int], Int]
        def solutionHelper(weight: Int, objects: List[Item], storage: Map[Pair[Int, Int], Int]): (Map[Pair[Int, Int], Int], Int) = {
            if(weight == 0) {
                (storage updated ((0, objects.size), 0), 0)
            }
            else if(storage.contains((weight, objects.size))) {
                (storage, storage((weight, objects.size)))
            }
            else {
                objects match {
                    case List() => {
                        (storage updated ((weight, 0), 0), 0)
                    }
                    case head :: tail => {
                        val cand1Res = if(weight >= head._2) solutionHelper((weight - head._2), tail, storage) else (storage, 0)
                        val cand1 = if(weight >= head._2) (cand1Res._1, head._1 + cand1Res._2) else cand1Res
                        val cand2 = solutionHelper(weight, tail, cand1._1)
                        val result = math.max(cand1._2, cand2._2)
                        (cand2._1 updated ((weight, objects.size), result), result)
                    }
                }
            }
        }
        solutionHelper(knapsackSize, items, optimals)._2
    }

    def computeOptimalSolution(url: String, recursive: Boolean = false): Int = {
        val lines = scala.io.Source.fromURL(url).getLines.toList
        val knapsackSize: Int = lines.head.split(" ")(0).toInt
        val items = lines.tail.collect({ case s: String =>
            s.split(" ") match {
                case Array(value, weight, _*) => (value.toInt, weight.toInt)
            }
        })
        if(recursive) computeOptimalSolutionRecursive(knapsackSize, items)
        else computeOptimalSolution(knapsackSize, items.toVector)
    }
}
