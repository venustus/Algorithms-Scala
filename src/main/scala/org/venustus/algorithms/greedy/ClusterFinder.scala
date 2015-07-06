package org.venustus.algorithms.greedy

import org.venustus.algorithms.graphs.Graph
import org.venustus.algorithms.unionfind.{UnionFind, OptimizedUnionFind}

/**
 * Created by venkat on 20/07/14.
 */
class ClusterFinder {

    /**
     * Max-spacing k-clustering problem.
     *
     * Given list of nodes and a distance function d(p, q) satisfying d(p, q) >= 0, d(p, p) = 0 & d(p, q) = d(q, p),
     * group the nodes into k coherent clusters. That is - find k clusters with maximum spacing, where spacing is
     * defined as minimum distance between any two points in the set of nodes, which are in separate clusters.
     *
     * We use the greedy approach here. This is very similar to kruskals minimum spanning tree algorithm except
     * that we stop once we attain k clusters.
     *
     * The input is represented by a graph where nodes are vertices and distance function is given by edge lengths.
     * Please note the input graph is fully connected - there is an edge between every pair of points.
     *
     * 1) We start with initializing a union-find data structure with all the available nodes - which means we start with
     *    n clusters for n nodes.
     * 2) We sort the edges by length and we loop over them picking one edge after another.
     * 3) For each edge, we combine the corresponding nodes in the union-find data structure using union operation.
     * 4) We do this until we have exactly k groups.
     *
     * Time complexity:
     *
     * @param edges
     * @param k
     * @return
     */
    def findKClusters(edges: List[Graph.Edge[Int]], k: Int) = {
        val nodes: Set[Int] = (edges map (_._1._1)).toSet union (edges map (_._1._2)).toSet
        val unionFindDS: UnionFind[Int] = OptimizedUnionFind[Int](nodes)
        val sortedEdges = edges sortBy ((e: Graph.Edge[Int]) => e._2)
        val result = ((((-1, -1), -1), unionFindDS) /: sortedEdges)((acc, e) => {
            acc match {
                case (((prev1, prev2), prevLength), ufds) => {
                    e match {
                        case ((e1, e2), el) => {
                            val (leader1, uf1) = ufds find (e1)
                            val (leader2, uf2) = uf1 find (e2)
                            if (leader1 == leader2) acc
                            else {
                                if (uf2.getLeaders.size == k) {
                                    if (prevLength == -1) (e, uf2) else (acc._1, uf2)
                                }
                                else (acc._1, uf2 union(e1, e2))
                            }
                        }
                    }
                }
            }
        })
        result._1._2
    }

    def findNumClustersForMinimumSpacing(nodes: List[Int], bitSize: Int) = {
        val nodeSet = nodes.toSet
        val onediffmasks = for(i <- 0 to bitSize - 1) yield Math.pow(2, i).toInt
        val twodiffmasks = for(i <- 0 to bitSize - 2; j <- 1 to bitSize - 1) yield Math.pow(2, i).toInt + Math.pow(2, j).toInt
        val onediffedges =
            for(node <- nodes;
                mask <- onediffmasks;
                otherNode = node ^ mask;
                if(node != otherNode && nodeSet.contains(otherNode))) yield ((node, otherNode), 1)
        val twodiffedges =
            for(node <- nodes;
                mask <- twodiffmasks;
                otherNode = node ^ mask;
                if(node != otherNode && nodeSet.contains(otherNode))) yield ((node, otherNode), 2)
        val allEdges = onediffedges ++ twodiffedges
        val unionFindDS = OptimizedUnionFind[Int](nodeSet)
        val result = allEdges.foldLeft[UnionFind[Int]](unionFindDS)((acc: UnionFind[Int], e: Graph.Edge[Int]) => {
            val (leader1, uf1) = acc find (e._1._1)
            val (leader2, uf2) = uf1 find (e._1._2)
            if(leader1 == leader2) acc
            else uf2 union (e._1._1, e._1._2)
        })
        result.getLeaders.size
    }
}
