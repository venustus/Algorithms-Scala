package org.venustus.algorithms.priorityqueues

import org.scalatest.{FlatSpec, Matchers}

import scala.collection.mutable.ArrayBuffer

/**
 * Created by venkat on 05/07/15.
 */
class PriorityQueueSpec extends FlatSpec with Matchers {

    "An array based priority queue implementation" should "be correct" in {
        val pq = PriorityQueue[Int](ArrayBuffer[Int](1, 2, 3, 4, 5))
        pq.head should be (5)
        pq.removeHead
        pq.head should be (4)

    }

}
