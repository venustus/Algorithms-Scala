package org.venustus.algorithms.lists

import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by venkat on 17/09/16.
  */
class ListSpec extends FlatSpec with Matchers {

    "Standard functions in linked list" should "behave as expected" in {
        an [IllegalStateException] should be thrownBy {
            MutableLinkedList(None) tail
        }
        var myList: LinkedList[Int] = MutableLinkedList(Some(ListElement(5, None)))
        (myList tail) should be (ListElement(5, None))
        ((myList insertInFront 4) getHead) should be (ListElement(4, Some(ListElement(5, None))))

        myList = (myList insertInFront 4) insertAtEnd 6
        (myList tail) should be (ListElement(6, None))

        val secondElem = myList find 5
        secondElem should be (Some(ListElement(5, Some(ListElement(6, None)))))

        myList = myList insertAfter (10, secondElem.get)

        (myList findMthToLastElement 2) should be (10)

        ((myList deleteElem secondElem.get) findMthToLastElement 3) should be (4)
    }

}
