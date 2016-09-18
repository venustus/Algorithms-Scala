package org.venustus.algorithms.lists

import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by venkat on 17/09/16.
  */
class CircularListsSpec extends FlatSpec with Matchers {

    "Algorithm" should "detect cycles correctly" in {
        val myList = LinkedList(List(1, 2, 3, 4, 5))
        (CircularLists detectCycle myList) should be (false)
        (myList tail) next = myList.getHead.next
        (CircularLists detectCycle myList) should be (true)
    }

}
