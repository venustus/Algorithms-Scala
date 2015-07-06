package org.venustus.algorithms.graphs

import org.scalatest.{FlatSpec, Matchers}
import org.venustus.algorithms.graphs.DijkstraShortestPaths.Distance

/**
 * Created by venkat on 05/07/15.
 */
class DijkstraShortestPathsSpec extends FlatSpec with Matchers {

    "Distance measure" should "be comparable properly" in {
        val ds = List(Distance[Int](1, 5), Distance[Int](2, 3), Distance[Int](3, 1241231), Distance[Int](4, 1241231), Distance[Int](5, 4))
        ds.sorted should be (List(Distance[Int](3, 1241231), Distance[Int](4, 1241231), Distance[Int](1, 5), Distance[Int](5, 4), Distance[Int](2, 3)))
    }

    "Basic dijkstra's algorithm" should "work" in {
        val g = new UndirectedGraph[Int](List(((1, 2), 24), ((1, 2), 20), ((1, 3), 3), ((1, 3), 2), ((1, 4), 20), ((3, 4), 12), ((5, 6), 100)))
        DijkstraShortestPaths.findAllShortestPathsFromNode(g, 1) should be (Map[Int, Option[Int]](1 -> Some(0), 2 -> Some(20), 3 -> Some(2), 4 -> Some(14), 5 -> None, 6 -> None))
    }

}
