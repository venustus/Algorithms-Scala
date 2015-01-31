package org.venustus.algorithms.spoj.scypher

import org.venustus.algorithms.graphs.DirectedGraph

/**
 * Created by venkat on 05/12/14.
 */
object SCypher {

    val alphabet = Vector('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z')

    /**
     * Given two strings, this does a lexicographic comparison of the strings
     * and returns first differentiating pair if one exists. If the strings
     * are exactly the same (upto the shortest length of the two), then this returns
     * None.
     * @param a
     * @param b
     * @return
     */
    def getDifferentiatingCharPair(a: String, b: String) = {
        def helper(zippedArr: List[(Char, Char)]): Option[Pair[Char, Char]] = {
            zippedArr match {
                case List() => None
                case x :: xs => x match {
                    case (y, z) =>
                        if (y.toInt != z.toInt) Some((y, z)) else helper(xs)
                }
            }
        }
        helper((a.toCharArray zip b.toCharArray).toList)
    }

    /**
     * Retrieves the key for a substitution cipher on a given alphabet just looking at a list of
     * encrypted words.
     * If it is not possible to break the cypher, this method returns None.
     * Algorithm:
     *
     * 1) Get all the differentiating char pairs indicating partial order among the alphabet.
     * 2) Construct a directed graph from these pairs - the individual characters become vertices and there is
     *    an edge between two chars if the two form a differentiating pair.
     * 3) Get a topological ordering of the vertices in this directed acyclic graph.
     * 4) Check that all the vertices in the topological order are connected to each other directly and if so,
     *    return the topological order of characters. Returns None otherwise.
     *
     * Time complexity:
     * Assuming maximum size of encrypted word is k and there are m words on an alphabet of size n.
     * Getting differentiating pairs for two strings takes O(k).
     * So, getting all differentiating pairs takes O(m * k).
     * Now the graph is of size m edges and n vertices. So, getting the topological ordering takes O(n).
     *
     * So, overall time complexity is O(n + m * k)
     *
     * @param alphabetSize
     * @param encryptedWords
     * @return
     */
    def retrieveCipherKey(alphabetSize: Int, encryptedWords: Vector[String]): Option[Vector[Char]] = {
        val charPairs = for {
            i <- 0 until encryptedWords.size - 1
        } yield getDifferentiatingCharPair(encryptedWords(i), encryptedWords(i + 1))
        val currentAlphabet: Vector[Char] = alphabet take alphabetSize
        val validCharPairs: List[((Char, Char), Int)]  = (List[((Char, Char), Int)]() /: charPairs)((acc, pair) => pair match {
            case Some(x) => (x, 1) :: acc
            case None => acc
        })

        val comparisonGraph = DirectedGraph[Char](validCharPairs)
        val sortOrder = comparisonGraph getTopologicalSortOrdering

        def checkTotalOrdering(chls: List[Char]): Boolean = {
            chls match {
                case x :: y :: tl => (comparisonGraph.nodes(x) contains ((y, 1))) && checkTotalOrdering(y :: tl)
                case _ => true
            }
        }

        if(sortOrder.size == currentAlphabet.size && checkTotalOrdering(sortOrder.toList)) Some(sortOrder)
        else None
    }

    /**
     * Given an alphabet of lower case letters taken as a prefix from a-z and a list of encrypted words
     * checks if the encryption key can be retrieved just by looking at the encrypted words using the
     * cipherRetreivalAlgo.
     * This also takes a new encrypted message. It returns the decrypted message if a key was retrieved, and
     * a string 'Message cannot be decrypted.' if not.
     * @param alphabetSize
     * @param encryptedWords
     * @param msg
     * @param cipherRetreivalAlgo
     * @return
     */
    def decryptSubstitutionCipher(alphabetSize: Int, encryptedWords: Vector[String], msg: String, cipherRetreivalAlgo: (Int, Vector[String]) => Option[Vector[Char]]): String = {
        val currentAlphabet: Vector[Char] = alphabet take alphabetSize
        val key = cipherRetreivalAlgo(alphabetSize, encryptedWords)
        key match {
            case None => "Message cannot be decrypted."
            case Some(ls) => msg map ((ch) => {
                val i = ls indexOf ch
                if(i < 0) ch else currentAlphabet(i)
            })
        }
    }
}

object Main extends App {
    override def main(args: Array[String]) {
        val numCases = readInt
        for (i <- 1 to numCases) {
            (readLine split (" ")) map (_.toInt) match {
                case Array(alphabetSize, numEncryptedWords) => {
                    val encryptedWords = (Vector[String]() /: (1 to numEncryptedWords))((words, _) => {
                        words :+ readLine
                    })
                    println(SCypher.decryptSubstitutionCipher(alphabetSize, encryptedWords, readLine, SCypher.retrieveCipherKey))
                }
                case _ => println("Malformed input")
            }
        }
    }
}
