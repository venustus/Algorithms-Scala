package org.venustus.algorithms.unionfind

/**
 * Created by venkat on 20/07/14.
 */
case class OptimizedUnionFind[T] private (val parents: Map[T, T], val sizes: Map[T, Int]) extends UnionFind[T] {

    override def getLeaders: Set[T] = {
        parents.keySet.foldLeft(Set[T]())((acc: Set[T], k: T) => acc union (Set[T](find(k)._1)))
    }

    override def find(obj: T): Pair[T, UnionFind[T]] = {
        val pathToLeader = findPath(obj, List[T]())
        if(pathToLeader.size <= 2) (pathToLeader.head, this)
        else {
            (pathToLeader.head,
             OptimizedUnionFind[T](pathToLeader.tail.tail.foldLeft(parents)((p, x) => p updated (x, pathToLeader.head)),
                                  sizes))
        }
    }

    def findPath(obj: T, acc: List[T]): List[T] = if(parents(obj) == obj) obj :: acc else findPath(parents(obj), obj :: acc)

    override def union(obj1: T, obj2: T): UnionFind[T] = {
        val (leader1: T, modifiedDS1: UnionFind[T]) = find(obj1)
        val (leader2: T, modifiedDS2: UnionFind[T]) = modifiedDS1.find(obj2)
        val updatedDS = modifiedDS2.asInstanceOf[OptimizedUnionFind[T]]
        val (bigger, smaller) = if(updatedDS.sizes(leader1) > updatedDS.sizes(leader2)) (leader1, leader2) else (leader2, leader1)
        OptimizedUnionFind[T](updatedDS.parents updated (smaller, bigger), updatedDS.sizes updated (bigger, sizes(bigger) + sizes(smaller)))
    }

    override def toString = parents.toString
}

object OptimizedUnionFind {
    def apply[T](initialSet: Set[T]): OptimizedUnionFind[T] = OptimizedUnionFind[T]((initialSet map ((x: T) => (x, x))).toMap, (initialSet map ((x: T) => (x, 1))).toMap)
}
