package org.venustus.algorithms.graphs

import org.venustus.algorithms.priorityqueues.PriorityQueue

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable

/**
 * Created by venkat on 05/07/15.
 */
object DijkstraShortestPaths extends App {
    /**
     * Given a graph and a source node, finds and returns shortest paths from <code>s</code>
     * to all other vertices in the graph.
     *
     * Approach: Dijkstra's greedy algorithm.
     *
     * 1) At any point in the algorithm we maintain two sets of vertices - processed
     *    vertices and unprocessed ones. In one step, we pick the vertex in the unprocessed
     *    set that has minimum distance from any vertex in the processed set.
     * 2) After we pick the vertex, we update the distance to the vertex by comparing the
     *    new distance with the old distance and updating if necessary.
     *
     * Because the greedy algorithm works (proof not included), each vertex is updated only once.
     * However, we need to maintain a priority queue of vertices that are currently unprocessed.
     * We use a priority queue because we need to retrieve the vertex with minimum distance value.
     *
     * Overall time complexity: O((m + n) log n) where n is the number of vertices in the graph
     * and m is the number of edges in the graph.
     * @param g
     * @param s
     * @tparam T
     */
    def findAllShortestPathsFromNode[T](g: Graph[T], s: T) = {
        // this is the structure on which we shall build the priority queue
        // distances are 0 for start vertex and infinity for everything else
        val distances = ArrayBuffer[Distance[T]](g.nodes.keySet.toSeq.map((n) => Distance[T](n, if(n == s) 0 else Int.MaxValue)): _*)
        // this is going to be our answer
        val distanceMap = (mutable.Map[T, Option[Int]]() /: distances)((acc, dist) => {
            acc(dist.node) = if(dist.d == Int.MaxValue) None else Some(dist.d)
            acc
        })
        // initialize the priority queue
        val q = PriorityQueue[Distance[T]](distances)
        // as long as there is something in the queue, we need to check
        while(!q.isEmpty) {
            // get the next vertex, whose computed distance is minimum from S
            val nextMin = q.head
            q.removeHead

            // this is the final answer for this vertex
            // once removed from queue, we are never gonna update it again
            distanceMap(nextMin.node) = if(nextMin.d == Int.MaxValue) None else Some(nextMin.d)
            // add the current node to visited set
            distanceMap(nextMin.node) match {
                case Some(nextMind) => {
                    // the current vertex has a path from S
                    // loop through all neighbors and update their distances
                    // so that next minimum takes this path into account
                    for (edge <- g.nodes(nextMin.node)) {
                        edge match {
                            case (neighbor: T, edgeCost: Int) => {
                                // new proposed distance to neighbor through current node
                                val newDist = nextMind + edgeCost
                                // if neighbor is not already visited and neighbors current distance is more than
                                // the new computed distance, update the neighbor in both the map
                                // and the queue
                                if (distanceMap(neighbor).isEmpty || distanceMap(neighbor).get > newDist) {
                                    q improveKey (Distance[T](neighbor, newDist))
                                    distanceMap(neighbor) = Some(newDist)
                                }
                            }
                        }
                    }
                }
                case _ => ()
            }
        }
        distanceMap
    }

    /**
     * This class helps us compare distances within the heap.
     * In the heap, we want to order the objects by their current computed distance to S.
     * But we also want them to be comparable by their node values. For example,
     * when removing a node, all we have is the key and we don't have the correct distance
     * measure. So, the distance objects must be comparable using equals.
     * @param node
     * @param d
     * @tparam T
     */
    case class Distance[T](node: T, d: Int) extends Ordered[Distance[T]] {

        /**
         * This is reverse of comparison of distances. More the distance, lesser priority it is
         * in the heap.
         * @param that
         * @return
         */
        def compare(that: Distance[T]) = that.d compare (this.d)


        /**
         * The equals comparison must only use the node part of distance and not the
         * distance measure.
         * @param other
         * @return
         */
        override def equals(other: Any) =
            other != null && other.isInstanceOf[Distance[T]] &&
                node.equals(other.asInstanceOf[Distance[T]].node)


        override def hashCode = node.hashCode

        override def toString = "(" + node.toString + ", " + d + ")"
    }

    override def main(args: Array[String]) {
        val numTestCases = readInt
        for(_ <- (1 to numTestCases)) {
            val Array(n, m) = readLine.split(" ").map(_.toInt)
            val g = new UndirectedGraph[Int](
                (List[Graph.Edge[Int]]() /: (1 to m))((acc, _) => {
                    val Array(from, to, cost) = readLine.split(" ").map(_.toInt)
                    ((from, to), cost) :: acc
                })
            )
            val s = readInt
            val completeG = (g /: (1 to n))((acc, i) => {
                if(!(acc.nodes contains (i))) acc.addNode(i)
                else acc
            })
            val distances = DijkstraShortestPaths.findAllShortestPathsFromNode(completeG, s)
            println((distances - s).keySet.toList.sorted.map((n) => distances(n) match { case Some(d) => d; case None => -1 }).mkString(" "))
        }
    }
}
