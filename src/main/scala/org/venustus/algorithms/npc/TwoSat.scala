package org.venustus.algorithms.npc

import org.venustus.algorithms.graphs.DirectedGraph

/**
 * Created by venkat on 24/08/14.
 */
object TwoSat {
    def isTwoSatisfiable(clauses: List[Pair[Int, Int]]): Boolean = {
        val implicationGraph: DirectedGraph[Int] =
            DirectedGraph[Int]((for (clause <- clauses)
                                yield List(Pair(Pair(-(clause._1), clause._2), 0), Pair(Pair(-(clause._2), clause._1), 0))).flatten)
        val comps = implicationGraph.getStronglyConnectedComponents
        (true /: comps)((acc, component) => {
            acc && (true /: component)((acc2, elem) => {
                acc2 && !(component contains (-elem))
            })
        })
    }
}
