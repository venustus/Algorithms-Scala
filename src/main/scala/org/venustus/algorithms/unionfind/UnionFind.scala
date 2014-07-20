package org.venustus.algorithms.unionfind

/**
 * Created by venkat on 20/07/14.
 */

/**
 * Defines a generic interface for a union find data structure.
 * A union find data structure maintains a set of clusters containing
 * objects of type T.
 * Clusters are represented by leaders which is a selected representative
 * for a given cluster.
 * It supports two operations. First is union - that unites two clusters into
 * a single cluster and second is find - that returns leader of a given object.
 * @tparam T
 */
trait UnionFind[T] {

    def getLeaders: Set[T]

    def union(obj1: T, obj2: T): UnionFind[T]

    def find(obj: T): Pair[T, UnionFind[T]]
}
