package org.venustus.algorithms.basic

import scala.io.Source
import scala.collection.immutable.{SortedMap, HashMap, Map}

/**
 * Created by venkat on 30/08/14.
 */
object WordCounter {

    /**
      * Given a file, count the number of words in that file.
      *
      * Time complexity: O(n)
      * Space complexity: O(n)
      *
      * Where n is the number of words in the file.
      *
      * @param filename
      * @return
      */
    def countWords(filename: String): Map[String, Int] = {
        val tokens = Source.fromFile(filename).mkString.split("\\s+")
        (SortedMap[String, Int]() /: tokens)((acc, token) =>
            if(acc contains token) acc updated (token, acc(token) + 1)
            else acc updated (token, 1)
        )
    }
}
