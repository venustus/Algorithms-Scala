package org.venustus.algorithms.npc

import scala.collection.immutable.BitSet

/**
 * Created by venkat on 15/08/14.
 * Represents an instance of travelling salesman problem.
 * Each city is represented by a pair of double values and
 * distance between cities is represented by the euclidian distance
 * between the points.items.size +
 */
class TSP(val cities: Vector[Pair[Float, Float]]) {

    def convertToInt(bits: BitSet) =
        (0 /: (0 until cities.size))((acc, elem) => {
            if(bits.contains(elem)) acc + (1 << elem) else acc
        })

    def print2DVector(v: Vector[Vector[Float]]) =
        v foreach ((vx) => {vx foreach((vy) => print(vy + " " )); println})

    def getEuclidianDistance(city1: Pair[Float, Float], city2: Pair[Float, Float]): Float =
        math.sqrt(math.pow(city1._1 - city2._1, 2) + math.pow(city1._2 - city2._2, 2)).toFloat

    def getOptimalTourCost: Float = {
        val totalNumSubsets = math.pow(2, cities.size).toInt
        val bitset: BitSet = BitSet((0 until cities.size): _*)
        val allSubsets = bitset.subsets
        val initialValue = (Vector.fill(totalNumSubsets, cities.size)(0.0.toFloat) /: allSubsets)((acc1, s) => {
            val subsetRepr: Int = convertToInt(s)
            (acc1 /: (0 until cities.size))((acc2, c) => {
                if(s.size == 1 && s.contains(0)) acc2 updated (subsetRepr, acc2(subsetRepr) updated (c, 0.0F))
                else acc2 updated (subsetRepr, acc2(subsetRepr) updated (c, Float.MaxValue))
            })
        })
        //print2DVector(initialValue)
        val dpArray = (initialValue /: (2 to cities.size))((acc, subproblemSize) => {
            (acc /: bitset.subsets(subproblemSize))((acc2, s) => {
                if(!s.contains(0)) acc2
                else {
                    (acc2 /: s)((acc3, j) => {
                        if(j == 0) acc3
                        else {
                            val sInt = convertToInt(s)
                            val candidates = for(k <- s;
                                                 if k != j;
                                                 sMinusJInt = convertToInt((s - j)))
                                             yield if(acc3(sMinusJInt)(k) == Float.MaxValue) Float.MaxValue
                                                   else (acc3(sMinusJInt)(k) + getEuclidianDistance(cities(k), cities(j)))
                            acc3 updated (sInt, acc3(sInt) updated (j, candidates.min))
                        }
                    })
                }
            })
        })
        val finalCandidates = for(j <- 1 until cities.size) yield dpArray(convertToInt(bitset))(j) + getEuclidianDistance(cities(j), cities(0))
        finalCandidates.min
    }
}


