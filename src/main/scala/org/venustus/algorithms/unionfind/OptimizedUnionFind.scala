package org.venustus.algorithms.unionfind

/**
 * Created by venkat on 20/07/14.
  *
  * Implements optimized union find data structure
 */
case class OptimizedUnionFind[T] private (parents: Map[T, T], ranks: Map[T, Int]) extends UnionFind[T] {

    override def getLeaders: Set[T] = {
        (Set[T]() /: parents.keySet)((acc: Set[T], k: T) => acc + (find(k) match { case (leader, _) => leader }))
    }

    override def find(obj: T): Pair[T, UnionFind[T]] = {
        val pathToLeader = findPath(obj, List[T]())
        if(pathToLeader.size <= 2) (pathToLeader.head, this)
        else {
            (pathToLeader.head,
             OptimizedUnionFind[T]((parents /: pathToLeader.tail.tail)((p, x) => p updated (x, pathToLeader.head)),
                                  ranks))
        }
    }

    def findPath(obj: T, acc: List[T]): List[T] = if(parents(obj) == obj) obj :: acc else findPath(parents(obj), obj :: acc)

    override def union(obj1: T, obj2: T): UnionFind[T] = {
        val (leader1: T, modifiedDS1: UnionFind[T]) = find(obj1)
        val (leader2: T, modifiedDS2: UnionFind[T]) = modifiedDS1.find(obj2)
        val updatedDS = modifiedDS2.asInstanceOf[OptimizedUnionFind[T]]
        val (largerRankLeader, smallerRankLeader) =
            if(updatedDS.ranks(leader1) > updatedDS.ranks(leader2)) (leader1, leader2) else (leader2, leader1)
        val sameRank = updatedDS.ranks(leader1) == updatedDS.ranks(leader2)
        OptimizedUnionFind[T](updatedDS.parents updated (smallerRankLeader, largerRankLeader),
                                if(sameRank) updatedDS.ranks updated (largerRankLeader, updatedDS.ranks(largerRankLeader) + 1)
                                else updatedDS.ranks)
    }

    override def toString = parents.toString
}

object OptimizedUnionFind {
    def apply[T](initialSet: Set[T]): OptimizedUnionFind[T] =
        OptimizedUnionFind[T]((initialSet map ((x: T) => (x, x))).toMap, (initialSet map ((x: T) => (x, 1))).toMap)
}
