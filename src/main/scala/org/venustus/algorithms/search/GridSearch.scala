package org.venustus.algorithms.search

/**
 * Created by venkat on 04/06/15.
 */
object GridSearch {

    def searchForGrid(bigger: Array[String], smaller: Array[String]): Boolean = {
        val potentialMatches = Set[(Int, Int)]()
        val firstToMatch = smaller(0)
        def getFirstRowMatches(acc: Set[(Int, Int)], stringToSearch: String): Set[(Int, Int)] = {
            val matchedIndex: Int = stringToSearch indexOfSlice (firstToMatch)
            if(matchedIndex < 0) acc
            else getFirstRowMatches(acc + ((matchedIndex, 1)), stringToSearch substring (matchedIndex + firstToMatch.size))
        }
        val finalists = (potentialMatches /: bigger)((acc, row) => {
            val winners = (acc filter { case (column, smallerRowToMatch) => {
                smallerRowToMatch >= smaller.size ||
                    ((row slice (column, column + smaller(smallerRowToMatch).size)) equals (smaller(smallerRowToMatch)))
            }}) map { case (column, rownum) => (column, rownum + 1)}
            winners ++ getFirstRowMatches(Set[(Int, Int)](), row)
        })
        (finalists filter { case (column, rownum) => rownum >= smaller.size }).size > 0
    }

}
