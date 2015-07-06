package org.venustus.algorithms.priorityqueues

import scala.collection.mutable.ArrayBuffer

/**
 * Created by venkat on 05/07/15.
 */
trait PriorityQueue[T] {

    /**
     * Returns head of the priority queue.
     * @return
     */
    def head: T

    /**
     * Removes the current head of the priority queue and returns a new priority queue.
     * @return
     */
    def removeHead: PriorityQueue[T]

    /**
     * Improves key for a specific element from the queue and returns the updated queue.
     * @param elem
     * @return
     */
    def improveKey(elem: T): PriorityQueue[T]

    def isEmpty: Boolean

}


object PriorityQueue {
    def apply[T <% Ordered[T]](elems: ArrayBuffer[T]) = new PriorityQueueImpl[T](elems)
}