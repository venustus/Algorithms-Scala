package org.venustus.algorithms.graphs

import scala.collection.immutable.HashMap
import org.venustus.algorithms.graphs.Graph.Neighbor

/**
 * Created by venkat on 13/07/14.
 */


abstract class Graph[T](nodesNEdges: Pair[Map[T, List[Neighbor[T]]], List[Graph.Edge[T]]]) {
    val nodes = nodesNEdges._1
    val edges = nodesNEdges._2
    val numVertices = nodes.size
    val numEdges = edges.size

    def isConnected: Boolean = {
        val visitedMap = new HashMap[T, Boolean]
        depthFirstIterate(nodes.head._1, visitedMap)._2.size == numVertices
    }

    def depthFirstIterate(start: T): List[T] = depthFirstIterate(start, new HashMap[T, Boolean])._2

    def depthFirstIterate(start: T, visitedMap: Map[T, Boolean]): (Map[T, Boolean], List[T]) = {
        if(visitedMap.getOrElse(start, false)) (visitedMap, List[T]())
        else {
            val (newVisitedMap, allRecursiveResults) =
                (nodes(start).foldLeft((visitedMap updated (start, true), List[T]()))((acc: (Map[T, Boolean], List[T]), neighbor: Neighbor[T]) => {
                    val (newVisitedMap, recursiveResult) = depthFirstIterate(neighbor._1, acc._1)
                    (newVisitedMap, acc._2 ::: recursiveResult)
                }))
            (newVisitedMap, start :: allRecursiveResults)
        }
    }

    /**
     * Utilizes Prim's greedy algorithm to compute minimum spanning tree of the given graph.
     * @return
     */
    def getMinimumSpanningTree(start: T): List[Graph.Edge[T]]
}

case class UndirectedGraph[T](edgeList: List[Graph.Edge[T]])
        extends Graph[T](Graph.constructAdjacencyList[T](edgeList, false)) {
    override def getMinimumSpanningTree(start: T): List[Graph.Edge[T]] = {
        if(!isConnected) List[Graph.Edge[T]]()
        def spanningTreeHelper(acc: (Set[T], List[Graph.Edge[T]])): (Set[T], List[Graph.Edge[T]]) = {
            if(acc._1.size == numVertices) acc
            else {
                val safeEdge = (edges filter ((e: Graph.Edge[T]) =>
                    ((acc._1 contains e._1._1) && !(acc._1 contains e._1._2))
                )) minBy ((e: Graph.Edge[T]) => e._2)
                spanningTreeHelper(((acc._1 union (Set[T](safeEdge._1._2))), safeEdge :: acc._2))
            }
        }
        spanningTreeHelper((Set[T](start), List[Graph.Edge[T]]()))._2
    }
}

object Graph {
    type Edge[T] = Pair[Pair[T, T], Int]
    type Neighbor[T] = Pair[T, Int]
    def constructAdjacencyList[T](edges: List[Graph.Edge[T]], directed: Boolean): (Map[T, List[Neighbor[T]]], List[Graph.Edge[T]]) = {
        val adjacencyList = new HashMap[T, List[Neighbor[T]]]
        edges.foldLeft[(Map[T, List[Neighbor[T]]], List[Graph.Edge[T]])]((adjacencyList, List[Graph.Edge[T]]()))(
            (acc: (Map[T, List[Neighbor[T]]], List[Graph.Edge[T]]), newEdge: Graph.Edge[T]) => {
            newEdge match {
                case Pair(Pair(a, b), cost) => {
                    if(directed) (acc._1 updated (a, (b, cost) :: acc._1.getOrElse(a, List[Neighbor[T]]())), ((a, b), cost) :: acc._2)
                    else (acc._1 updated (a,
                            (b, cost) :: acc._1.getOrElse(a, List[Neighbor[T]]())) updated (b,
                                (a, cost) :: acc._1.getOrElse(b, List[Neighbor[T]]())), ((a, b), cost) :: ((b, a), cost) :: acc._2)
                }
                case _ => acc
            }
        })
    }
}

