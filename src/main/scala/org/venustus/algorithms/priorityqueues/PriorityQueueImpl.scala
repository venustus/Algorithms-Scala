package org.venustus.algorithms.priorityqueues

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable

/**
 * Created by venkat on 05/07/15.
 */
class PriorityQueueImpl[T <% Ordered[T]](private val elems: ArrayBuffer[T]) extends PriorityQueue[T] {

    // this map is crucial for us to be able to improve a given key
    // it stores the reverse mapping between the key and the index of the element in the array
    private val indexMap = mutable.Map[T, Int]()
    for(i <- Range(elems.size - 1, -1, -1)) heapifyDown(i)
    (indexMap /: elems.zipWithIndex)((acc, ith) => {
        acc(ith._1) = ith._2; acc
    })

    private def leftChildIndex(i: Int) = 2 * i + 1
    private def rightChildIndex(i: Int) = 2 * i + 2
    private def parent(i: Int) = if(i == 0) i else (i - 1) / 2

    private def swap(i: Int, j: Int) = {
        indexMap(elems(i)) = j
        indexMap(elems(j)) = i
        val temp = elems(i)
        elems(i) = elems(j)
        elems(j) = temp
    }

    private def heapifyUp(i: Int): Unit =
        if(i > 0 && elems(i) > elems(parent(i))) {
            swap(i, parent(i))
            heapifyUp(parent(i))
        }

    private def heapifyDown(i: Int): Unit = {
        if(i < elems.size) {
            val lci = leftChildIndex(i)
            val rci = rightChildIndex(i)

            var largest = i
            if(lci < elems.size && elems(largest) < elems(lci)) largest = lci
            if(rci < elems.size && elems(largest) < elems(rci)) largest = rci
            if(largest != i) {
                swap(i, largest)
                heapifyDown(largest)
            }
        }
    }


    /**
     * Returns head of the priority queue.
     * @return
     */
    def head: T = elems(0)

    /**
     * Removes the current head of the priority queue and returns the priority queue.
     * @return
     */
    def removeHead: PriorityQueue[T] = {
        swap(0, elems.size - 1)
        indexMap -= elems(elems.size - 1)
        elems reduceToSize (elems.size - 1)
        heapifyDown(0)
        this
    }

    /**
     * Improves the key for a specific element from the queue and returns the updated queue.
     * This may internally reorder the queue such that the element is now in its correct
     * position satisfying heap invariant.
     * @param newElem
     * @return
     */
    override def improveKey(newElem: T): PriorityQueue[T] = {
        val indexToUpdate = indexMap(newElem)
        elems(indexToUpdate) = newElem
        heapifyUp(indexToUpdate)
        this
    }

    def isEmpty = elems.isEmpty
}
