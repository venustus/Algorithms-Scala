package org.venustus.algorithms.search

/**
 * Created by venkat on 03/06/15.
 */
object ConnectedGridCells {

    def findLargestConnectedRegion(grid: Array[Array[Int]]) = {
        val visited = Array.fill[Boolean](grid.size, grid(0).size)(false)
        def getSizeOfConnectedRegion(vert: Int, hori: Int): Int = {
            var currentSize = 1
            visited(vert)(hori) = true
            if(vert > 0 && !visited(vert - 1)(hori) && grid(vert - 1)(hori) == 1) {
                currentSize = currentSize + getSizeOfConnectedRegion(vert - 1, hori)
            }
            if(vert < grid.size - 1 && !visited(vert + 1)(hori) && grid(vert + 1)(hori) == 1) {
                currentSize = currentSize + getSizeOfConnectedRegion(vert + 1, hori)
            }
            if(hori > 0 && !visited(vert)(hori - 1) && grid(vert)(hori - 1) == 1) {
                currentSize = currentSize + getSizeOfConnectedRegion(vert, hori - 1)
            }
            if(hori < grid(0).size - 1 && !visited(vert)(hori + 1) && grid(vert)(hori + 1) == 1) {
                currentSize = currentSize + getSizeOfConnectedRegion(vert, hori + 1)
            }
            if(vert > 0 && hori > 0 && !visited(vert - 1)(hori - 1) && grid(vert - 1)(hori - 1) == 1) {
                currentSize = currentSize + getSizeOfConnectedRegion(vert - 1, hori - 1)
            }
            if(vert < grid.size - 1 && hori > 0 && !visited(vert + 1)(hori - 1) && grid(vert + 1)(hori - 1) == 1) {
                currentSize = currentSize + getSizeOfConnectedRegion(vert + 1, hori - 1)
            }
            if(vert < grid.size - 1 && hori < grid(0).size - 1 && !visited(vert + 1)(hori + 1) && grid(vert + 1)(hori + 1) == 1) {
                currentSize = currentSize + getSizeOfConnectedRegion(vert + 1, hori + 1)
            }
            if(vert > 0 && hori < grid(0).size - 1 && !visited(vert - 1)(hori + 1) && grid(vert - 1)(hori + 1) == 1) {
                currentSize = currentSize + getSizeOfConnectedRegion(vert - 1, hori + 1)
            }
            currentSize
        }
        val allPositions = for {
            vertIndex <- 0 until grid.size
            horiIndex <- 0 until grid(0).size
        } yield (vertIndex, horiIndex)
        (0 /: allPositions)((acc, position) => {
            if(!visited(position._1)(position._2) && grid(position._1)(position._2) == 1) {
                math.max(acc, getSizeOfConnectedRegion(position._1, position._2))
            }
            else acc
        })
    }

}
