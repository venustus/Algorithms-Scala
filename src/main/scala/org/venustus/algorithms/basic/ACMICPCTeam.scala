package org.venustus.algorithms.basic

/**
 * Created by venkat on 01/02/15.
 */

class Person (val index: Int, val capabilities: String) {
    private val topics: List[Boolean] = (capabilities map (_ == '1')).toList

    def computeKnownTopics(other: Person) = (0 /: (topics zip other.topics))((acc, pair) => {
        pair match {
            case (b1, b2) => acc + (if(b1 || b2) 1 else 0)
        }
    })

    override def toString = index.toString

    def ==(other: Person) = index == other.index

    def <(other: Person) = index < other.index
}
object ACMICPCTeam {

    /**
     * A list of binary strings representing capabilities of a person are given.
     * Each string represents one person and each bit in a string represents one topic.
     * ith bit in jth string is 1 if ith person knows jth topic.
     *
     * Goal is to find maximum number of topics a two person team can know and also to find
     * number of such teams knowing maximum number of topics.
     *
     * Algorithm:
     *
     * 1) First convert the given strings into a convenient data structure. O(n * m)
     * 2) Then run a double for loop and for each pair of people, compute the number
     *    of known topics to either of them and store it in a list. O(n^2 * m)
     * 3) Sort the list in descending order and pick up the first number. O(n^2 log n)
     * 4) The number of items in this list which are same as first number gives us number of such teams.
     *
     * Time complexity: O(n^2 * (m + log n))
     * Space complexity: O(n^2 * m)
     * @param personsCapabilities
     * @return
     */
    def findMaxTopicPairs(personsCapabilities: List[String]): Pair[Int, Int] = {
        val people = (personsCapabilities zipWithIndex) map { case (s, i) => new Person(i + 1, s) }
        val allPairs = for {
            i <- people
            j <- people
            if(i < j)
        } yield (i computeKnownTopics (j))

        val allPairsDesc = allPairs sortBy (- _)
        val maxTopicCount = allPairsDesc.head
        (maxTopicCount, (allPairsDesc takeWhile (_ == maxTopicCount)).size)
    }

}
