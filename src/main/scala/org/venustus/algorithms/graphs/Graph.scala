package org.venustus.algorithms.graphs

import org.venustus.algorithms.graphs.Graph.Neighbor
import org.venustus.algorithms.unionfind.{UnionFind, OptimizedUnionFind}

/**
 * Created by venkat on 13/07/14.
 */

object Color extends Enumeration {
    type Color = Value
    val WHITE, GRAY, BLACK = Value
}
import Color._

abstract class Graph[T](nodesNEdges: Pair[scala.collection.Map[T, List[Graph.Neighbor[T]]], List[Graph.Edge[T]]], isDirected: Boolean) {
    val nodes = nodesNEdges._1
    val edges = nodesNEdges._2
    val numVertices = nodes.size
    val numEdges = edges.size
    val directed: Boolean = isDirected

    def addNode(t: T): Graph[T]

    def addEdge(e: Graph.Edge[T]): Graph[T]

    def isConnected: Boolean = {
        depthFirstIterate(List[T](nodes.head._1), new scala.collection.mutable.HashMap[T, Color], Vector[T]())._2.size == numVertices
    }

    def depthFirstIterate(start: T): Vector[T] = depthFirstIterate(List[T](start), new scala.collection.mutable.HashMap[T, Color], Vector[T]())._2

    @annotation.tailrec
    final def depthFirstIterate(toVisit: List[T], visitedMap: scala.collection.mutable.HashMap[T, Color],
                                dfsOrder: Vector[T], finishTimesOrder: Vector[T] = Vector[T]()): (scala.collection.mutable.HashMap[T, Color], Vector[T], Vector[T]) = {
        toVisit match {
            case List() => (visitedMap, dfsOrder, finishTimesOrder)
            case hd :: tl => {
                if(visitedMap contains (hd)) {
                    if(visitedMap(hd) == GRAY) {
                        visitedMap put (hd, BLACK)
                        depthFirstIterate(tl, visitedMap, dfsOrder, hd +: finishTimesOrder)
                    }
                    else depthFirstIterate(tl, visitedMap, dfsOrder, finishTimesOrder)
                }
                else {
                    val unvisitedNeighbors: List[T] = nodes(hd).map(_._1).filterNot((p) => visitedMap contains(p))
                    visitedMap put (hd, GRAY)
                    depthFirstIterate((unvisitedNeighbors :+ hd) ++ tl, visitedMap, dfsOrder :+ toVisit.head, finishTimesOrder)
                }
            }
        }
    }

    def depthFirstSearch: (Vector[T], Vector[T]) = {
        val (_, dfsOrder, finishTimesOrder) = ((new scala.collection.mutable.HashMap[T, Color], Vector[T](), Vector[T]()) /: nodes){ case (acc, (curVertex, _)) => {
            acc match {
                case (visitedMap, depthFirstOrder, finishTimesOrder) => {
                    if(visitedMap contains curVertex) acc
                    else depthFirstIterate(List(curVertex), visitedMap, depthFirstOrder, finishTimesOrder)
                }
            }
        }}
        (dfsOrder, finishTimesOrder)
    }

    /**
     * Utilizes Prim's greedy algorithm to compute minimum spanning tree of the given graph.
     * @return
     */
    def getMinimumSpanningTree(start: T): List[Graph.Edge[T]] = {
        if(!isConnected) List[Graph.Edge[T]]()
        def spanningTreeHelper(acc: (Set[T], List[Graph.Edge[T]])): (Set[T], List[Graph.Edge[T]]) = {
            if(acc._1.size == numVertices) acc
            else {
                val safeEdge = (edges filter ((e: Graph.Edge[T]) =>
                    ((acc._1 contains e._1._1) && !(acc._1 contains e._1._2)) ||
                            (!directed && (acc._1 contains e._1._2) && !(acc._1 contains e._1._1))
                        )) minBy ((e: Graph.Edge[T]) => e._2)
                val additionalVertex = if(acc._1 contains safeEdge._1._1) safeEdge._1._2 else safeEdge._1._1
                spanningTreeHelper(((acc._1 union (Set[T](additionalVertex))), safeEdge :: acc._2))
            }
        }
        spanningTreeHelper((Set[T](start), List[Graph.Edge[T]]()))._2
    }

    /**
     * Utilizes kruskals greedy algorithm to find minimum spanning tree
     * @return
     */
    def getMinimumSpanningTree(): List[Graph.Edge[T]] = {
        val unionFindDS: UnionFind[T] = OptimizedUnionFind(nodes.keySet.toSet)
        val sortedEdges = edges sortBy ((e: Graph.Edge[T]) => e._2)
        val result = ((List[Graph.Edge[T]](), unionFindDS) /: sortedEdges)((acc, e) =>{
            val (result, ds) = acc
            val ((vertex1, vertex2), _) = e

            val (leader1, uf1) = ds find (vertex1)
            val (leader2, uf2) = uf1 find (vertex2)
            if(leader1 != leader2) {
                (e :: result, uf2 union (vertex1, vertex2))
            }
            else acc
        })
        result._1
    }

    /**
     * Uses Floyd-warshall algorithm to compute shortest paths between
     * all pairs of vertices in the given graph.
     * @return a list of pairs - each pair is a combination of a pair of vertices and the corresponding
     *         shortest path
     */
    def getAllPairsShortestPaths: List[Pair[Pair[T, T], Int]] = {
        val orderedVertices = nodes.keySet.toVector
        val emptyMatrix = Vector.fill(numVertices + 1, numVertices + 1)(0)
        val firstRound = (emptyMatrix /: (1 to numVertices))((m1, i) => {
            (m1 /: (1 to numVertices))((m2, j) => {
                // shortest path from i to i using no vertices is 0
                if(i == j) m2 updated (i, (m2(i) updated (j, 0)))
                // shortest path from i to j using no vertices at all is length of edge between i and j
                // if one exists
                else {
                    val edge: Option[Neighbor[T]] = nodes(orderedVertices(i - 1)).find((_._1 == orderedVertices(j - 1)))
                    edge match {
                        case Some(neighbor) => m2 updated (i, (m2(i) updated (j, neighbor._2)))
                        case None => m2 updated (i, (m2(i) updated (j, Int.MaxValue)))
                    }
                }
            })
        })
        // main for loop
        val resultMatrix = (firstRound /: (1 to numVertices))((aPrev, k) => {
            (emptyMatrix /: (1 to numVertices))((m1, i) => {
                (m1 /: (1 to numVertices))((m2, j) => {
                    m2 updated (i,
                            (m2(i) updated (j,
                                    math.min(aPrev(i)(j),
                                        if(aPrev(i)(k) == Int.MaxValue || aPrev(k)(j) == Int.MaxValue) Int.MaxValue
                                        else  aPrev(i)(k) + aPrev(k)(j)))))
                })
            })
        })
        for(i <- 1 to numVertices) {
            if(resultMatrix(i)(i) < 0) throw new NegativeCostCycleException
        }
        (List[Pair[Pair[T, T], Int]]() /: (1 to numVertices))((res1, i) => {
            (res1 /: (1 to numVertices))((res2, j) => {
                ((orderedVertices(i - 1), orderedVertices(j - 1)), resultMatrix(i)(j)) :: res2
            })
        })
    }
}

case class UndirectedGraph[T](nodesAndEdges: Pair[scala.collection.Map[T, List[Graph.Neighbor[T]]], List[Graph.Edge[T]]])
        extends Graph[T](nodesAndEdges, false) {

    def this(edgeList: List[Graph.Edge[T]]) = {
        this(Graph.constructAdjacencyList[T](edgeList, false))
    }

    override def addNode(t: T): UndirectedGraph[T] = new UndirectedGraph((nodes updated (t, List()), edges))

    override def addEdge(e: Graph.Edge[T]): UndirectedGraph[T] = {
        e match {
            case ((from, to), weight) => {
                val neighbors = nodes getOrElse (from, List())
                val reverseNeighbors = nodes getOrElse (to, List())
                new UndirectedGraph(
                    ((nodes updated (from, (to, weight) :: neighbors)) updated (to, (from, weight) :: reverseNeighbors),
                    e :: edges))
            }
        }
    }
}

class DirectedGraph[T](nodesAndEdges: Pair[scala.collection.Map[T, List[Neighbor[T]]], List[Graph.Edge[T]]])
        extends Graph[T](nodesAndEdges, true) {

    def this(edgeList: List[Graph.Edge[T]]) = {
        this(Graph.constructAdjacencyList[T](edgeList, true))
    }

    val nodesReverse = (new scala.collection.mutable.HashMap[T, List[Neighbor[T]]] /: edges)((acc, edge) => {
        val ((vertex1, vertex2), cost) = edge
        acc put (vertex2, (vertex1, cost) :: (acc getOrElse (vertex2, List())))
        acc
    })

    override def addNode(t: T) = new DirectedGraph((nodes updated (t, List()), edges))

    override def addEdge(e: Graph.Edge[T]) = {
        e match {
            case ((from, to), weight) => {
                val neighbors = nodes getOrElse (from, List())
                new DirectedGraph(
                    (nodes updated (from, (to, weight) :: neighbors),
                    e :: edges))
            }
        }
    }

    def getFinishingTimes(start: T): Vector[T] =
        depthFirstIterateReverse(List[T](start), new scala.collection.mutable.HashMap[T, Color], Vector[T]())._2

    @annotation.tailrec
    final def depthFirstIterateReverse(toVisit: List[T], visitedMap: scala.collection.mutable.HashMap[T, Color], finishingTimes: Vector[T]): (scala.collection.mutable.HashMap[T, Color], Vector[T]) = {
        toVisit match {
            case List() => (visitedMap, finishingTimes)
            case hd :: tl => {
                if(visitedMap contains hd) {
                    if(visitedMap(hd) == GRAY) {
                        visitedMap put (hd, BLACK)
                        depthFirstIterateReverse(tl, visitedMap, hd +: finishingTimes)
                    }
                    else depthFirstIterateReverse(tl, visitedMap, finishingTimes)
                }
                else {
                    if(!nodesReverse.contains(hd)) {
                        visitedMap put (hd, BLACK)
                        depthFirstIterateReverse(tl, visitedMap, hd +: finishingTimes)
                    }
                    else {
                        val unvisitedNeighbors: List[T] = nodesReverse(hd).map(_._1).filterNot((p) => visitedMap contains(p))
                        visitedMap put (hd, GRAY)
                        depthFirstIterateReverse((unvisitedNeighbors :+ hd) ++ tl, visitedMap, finishingTimes)
                    }
                }
            }
        }
    }

    /**
     * Performs a reverse depth first search on the given graph.
     * This means, incident of following edges out of a vertex, we follow
     * edges that are incident on the vertex to move forward in a depth first
     * search.
     * @return an ordered list of vertices in descending order of finishing times - vertex that finished last
     *         is at head of the list
     */
    def depthFirstSearchReverse: Vector[T] = {
        val result = ((new scala.collection.mutable.HashMap[T, Color], Vector[T]()) /: nodes){ case(acc, (curVertex, _)) => {
            acc match {
                case (visitedMap, finishingTimes) => {
                    if(visitedMap contains curVertex) acc
                    else {
                        depthFirstIterateReverse(List[T](curVertex), visitedMap, finishingTimes)
                    }
                }
            }
        }}
        result._2
    }

    def getStronglyConnectedComponents: Set[Set[T]] = {
        val desiredDFSOrder = depthFirstSearchReverse
        val result = ((new scala.collection.mutable.HashMap[T, Color], Set[Set[T]]()) /: desiredDFSOrder)((acc, nodeVal) => {
            acc match {
                case (visitedMap, results) => {
                    if(visitedMap contains (nodeVal)) acc
                    else {
                        depthFirstIterate(List[T](nodeVal), visitedMap, Vector[T]()) match {
                            case (visitedMap, resultList, finishTimes) => (visitedMap, results + resultList.toSet)
                            case _ => acc
                        }
                    }
                }
                case _ => acc
            }
        })
        result._2
    }


    def getTopologicalSortOrdering: Vector[T] = {
        val (_, finishTimesOrder) = depthFirstSearch
        finishTimesOrder
    }
}

case class Tree[T](edgeList: List[Graph.Edge[T]])
        extends DirectedGraph[T](edgeList) {
    private val potentialRoot = nodes find { case (n, edges) => {
            edges.size == 0
        }
    }
    val root = potentialRoot match {
        case None => throw new IllegalArgumentException("Invalid input for a tree")
        case Some(x) => x._1
    }
}

object Graph {
    type Edge[T] = Pair[Pair[T, T], Int]
    type Neighbor[T] = Pair[T, Int]
    def constructAdjacencyList[T](edges: List[Graph.Edge[T]], directed: Boolean): (scala.collection.Map[T, List[Neighbor[T]]], List[Graph.Edge[T]]) = {
        val adjacencyList = new scala.collection.mutable.HashMap[T, List[Neighbor[T]]]
        ((adjacencyList, List[Graph.Edge[T]]()) /: edges)((acc, newEdge) => {
            newEdge match {
                case Pair(Pair(a, b), cost) => {
                    val (adjList, edges) = acc
                    adjList put (a, (b, cost) :: (adjList getOrElse (a, List())))
                    if(directed) adjList put (b, adjList getOrElse (b, List()))
                    else adjList put (b, (a, cost) :: (adjList getOrElse (b, List())))
                    (adjList, ((a, b), cost) :: edges)
                }
                case _ => acc
            }
        })
    }
}

class NegativeCostCycleException extends Exception {
    override def getMessage = "Negative cost cycle exists in the graph."
}

