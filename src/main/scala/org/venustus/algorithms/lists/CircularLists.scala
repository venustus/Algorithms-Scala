package org.venustus.algorithms.lists

/**
  * Created by venkat on 17/09/16.
  */
object CircularLists {

    /**
      * Returns true iff a given linked list has a cycle.
      * @param l
      * @tparam T
      * @return
      */
    def detectCycle[T](l: LinkedList[T]) = {
        if(l.head.isEmpty || l.getHead.next.isEmpty) true
        else {
            var fastPointer = l.getHead.next
            var slowPointer = l.head

            while(fastPointer.isDefined && slowPointer.isDefined && !(fastPointer.get eq slowPointer.get)) {
                fastPointer = fastPointer.get.next
                if(fastPointer.isDefined) fastPointer = fastPointer.get.next
                slowPointer = slowPointer.get.next
            }
            fastPointer.isDefined
        }
    }

    /**
      * Removes head of a perfectly circular singly-linked list.
      *
      * This executes in O(1) time (that is it does not traverse to the end of the list).
      * O(n) algorithm is trivial, but this one is clever.
      *
      * @param l
      * @tparam T
      * @return
      */
    def removeHeadOfACircularList[T](l: LinkedList[T]) ={
        if(l.head.isEmpty) throw new IllegalArgumentException("List is already empty")
        if(l.getHead.next.isEmpty) MutableLinkedList(None)
        else {
            // copy over data of the second element into the head and remove the second element
            // instead of removing the head (which is not possible in a singly linked list)
            l.getHead.data = l.getHead.next.get.data
            l.getHead.next = l.getHead.next.get.next
            l
        }
    }
}
