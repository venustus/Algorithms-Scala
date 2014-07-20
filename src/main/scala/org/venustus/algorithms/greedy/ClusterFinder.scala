package org.venustus.algorithms.greedy

import org.venustus.algorithms.graphs.Graph
import org.venustus.algorithms.unionfind.{UnionFind, OptimizedUnionFind}

/**
 * Created by venkat on 20/07/14.
 */
class ClusterFinder {
    def findKClusters(edges: List[Graph.Edge[Int]], k: Int) = {
        val nodes: Set[Int] = (edges map (_._1._1)).toSet union (edges map (_._1._2)).toSet
        val unionFindDS = OptimizedUnionFind[Int](nodes)
        val sortedEdges = edges sortBy ((e: Graph.Edge[Int]) => e._2)
        val result = sortedEdges.foldLeft[Pair[Graph.Edge[Int], UnionFind[Int]]]((((-1, -1), -1), unionFindDS))((acc: (Graph.Edge[Int], UnionFind[Int]), e: Graph.Edge[Int]) => {
            val (leader1, uf1) = acc._2 find (e._1._1)
            val (leader2, uf2) = uf1 find (e._1._2)
            if(leader1 == leader2) acc
            else {
                if(uf2.getLeaders.size == k) {
                    if(acc._1._2 == -1) (e, uf2) else (acc._1, uf2)
                }
                else (acc._1, uf2 union (e._1._1, e._1._2))
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
