package org.venustus.algorithms.search

import scala.collection.mutable.{ArrayBuffer, LinkedList}

/**
 * Created by venkat on 03/06/15.
 */
object CountLuck {

    def isHermioneSaved(grid: Array[Array[Char]], kMax: Int) = {
        val visited = Array.fill[Boolean](grid.size, grid(0).size)(false)
        def exploreFromPoint(row: Int, column: Int, currentK: Int): Boolean = {
            visited(row)(column) = true
            if(grid(row)(column) == '*') {
                if (currentK == kMax) true
                else false
            }
            else {
                val paths: ArrayBuffer[(Int, Int)] = new ArrayBuffer[(Int, Int)]()
                if (row > 0 && !visited(row - 1)(column) && grid(row - 1)(column) != 'X') {
                    paths += ((row - 1, column))
                }
                if (row < grid.size - 1 && !visited(row + 1)(column) && grid(row + 1)(column) != 'X') {
                    paths += ((row + 1, column))
                }
                if (column > 0 && !visited(row)(column - 1) && grid(row)(column - 1) != 'X') {
                    paths += ((row, column - 1))
                }
                if (column < grid(0).size - 1 && !visited(row)(column + 1) && grid(row)(column + 1) != 'X') {
                    paths += ((row, column + 1))
                }
                if(paths.size == 0) false
                else if(paths.size == 1) exploreFromPoint(paths(0)._1, paths(0)._2, currentK)
                else {
                    (false /: paths)((res, point) => {
                        res || exploreFromPoint(point._1, point._2, currentK + 1)
                    })
                }
            }
        }
        var startingPoint: Option[(Int, Int)] = None
        for {
            i: Int <- 0 until grid.size
            j: Int <- 0 until grid(0).size
        } {
            if(grid(i)(j) == 'M') startingPoint = Some((i, j))
        }
        startingPoint match {
            case None => throw new IllegalArgumentException("Hermione Granger is missing!")
            case Some((r, c)) => exploreFromPoint(r, c, 0)
        }
    }

}
