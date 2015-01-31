package org.venustus.algorithms.dp

import org.venustus.algorithms.greedy.ClusterFinder
import scala.collection.immutable.HashMap

/**
 * Created by venkat on 27/07/14.
 */
object Knapsack {

    type Item = Pair[Int, Int]

    def computeOptimalSolution(knapsackSize: Int, items: Vector[Item]): Int = {
        val finalResult = (1 to items.size).foldLeft(Vector.fill(items.size + 1, knapsackSize + 1)(0))((result, i) =>{
            (1 to knapsackSize).foldLeft(result)((res, w) => {
                res updated (i, res(i) updated(w, math.max({if(w < items(i - 1)._2) 0 else (items(i - 1)._1 + res(i - 1)(w - items(i - 1)._2))}, res(i - 1)(w))))
            })
        })
        finalResult(items.size)(knapsackSize)
    }

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
