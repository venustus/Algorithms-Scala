package org.venustus.algorithms.lists


trait LinkedList[T] {

    def isEmpty: Boolean

    def head: Option[ListElement[T]]

    def getHead: ListElement[T]

    def insertInFront(data: T): LinkedList[T]

    def tail: ListElement[T]

    def insertAtEnd(data: T): LinkedList[T]

    def find(data: T): Option[ListElement[T]]

    def deleteElem(elemToDelete: ListElement[T]): LinkedList[T]

    def insertAfter(dataToInsert: T, referenceElem: ListElement[T]): LinkedList[T]

    def findMthToLastElement(m: Int): T
}

/**
  * Created by venkat on 17/09/16.
  *
  * Traditional mutable linked list implementation in Scala.
  */
case class ListElement[T] (var data: T, var next: Option[ListElement[T]])

case class MutableLinkedList[T](headNode: Option[ListElement[T]]) extends LinkedList[T] {

    def isEmpty = headNode.isEmpty

    def head = headNode

    /**
      * Constant time operation - returns a new list.
      * @param data data to be inserted
      * @return
      */
    override def insertInFront(data: T) = headNode match {
        case None => MutableLinkedList(Some(ListElement(data, None)))
        case Some(t) => MutableLinkedList(Some(ListElement(data, headNode)))
    }

    override def getHead = headNode match {
        case None => throw new IllegalStateException("Head of an empty list")
        case Some(h) => h
    }

    /**
      * Returns the last element of the list.
      * Time complexity: O(n)
      * @return
      */
    override def tail = {
        def tailHelper(elem: ListElement[T]): ListElement[T] = elem.next match {
            case None => elem
            case Some(t) => tailHelper(t)
        }
        headNode match {
            case None => throw new IllegalStateException("Cannot retrieve tail of an empty list")
            case Some(le) => tailHelper(le)
        }
    }

    /**
      * Inserts a new element with {@code data} as data at the end of the list
      * and returns the same list.
      *
      * Time complexity: O(n)
      * @param data
      * @return
      */
    override def insertAtEnd(data: T): LinkedList[T] = {
        try {
            val tailElem = tail
            tailElem.next = Some(ListElement(data, None))
            this
        } catch {
            case ex: IllegalStateException => MutableLinkedList(Some(ListElement(data, None)))
        }
    }

    /**
      * Searches this linked list for a given data.
      * Time complexity: O(n)
      *
      * @param data data to search for
      * @return None if the element is not found and Some(t) if it is found
      */
    override def find(data: T): Option[ListElement[T]] = {
        def findHelper(elem: ListElement[T]): Option[ListElement[T]] = {
            if(elem.data == data) Some(elem)
            else {
                elem.next match {
                    case None => None
                    case Some(t) => findHelper(t)
                }
            }
        }
        headNode match {
            case None => None
            case Some(t) => findHelper(t)
        }
    }

    /**
      * Deletes an element by reference from the current list
      * and returns the modified list.
      *
      * Time complexity: O(n) worst case
      *
      * @param elemToDelete a reference to element to delete
      * @return
      * @throws IllegalStateException when the list is empty
      */
    override def deleteElem(elemToDelete: ListElement[T]): LinkedList[T] = {
        def deleteHelper(elem: ListElement[T]): Unit = elem.next match {
            case None =>
            case Some(t) =>
                if(t eq elemToDelete) {
                    elem.next = elemToDelete.next
                } else {
                    deleteHelper(t)
                }
        }
        headNode match {
            case None => throw new IllegalStateException("Cannot delete from empty list")
            case Some(t) =>
                if(t eq elemToDelete) MutableLinkedList(t.next)
                else {
                    deleteHelper(t)
                    this
                }
        }
    }

    /**
      * Inserts a new data entry after a given reference element.
      *
      * Time complexity: O(n)
      *
      * @param dataToInsert
      * @param referenceElem
      * @return
      */
    override def insertAfter(dataToInsert: T, referenceElem: ListElement[T]): LinkedList[T] = {
        def insertHelper(elem: Option[ListElement[T]]): Unit = elem match {
            case None =>
            case Some(t) =>
                if(t eq referenceElem) {
                    t.next = Some(ListElement(dataToInsert, t.next))
                } else insertHelper(t.next)
        }
        insertHelper(headNode)
        this
    }

    /**
      * Finds mth element from the end. Maintains two pointers one trailing the other,
      * m positions behind.
      * @param m
      * @return
      * @throws IllegalStateException if there are not enough elements in the list
      */
    override def findMthToLastElement(m: Int): T = {
        def getNthElementFromBeginning(n: Int, curHead: Option[ListElement[T]]): ListElement[T] = curHead match {
            case None => throw new IllegalStateException("List is not long enough")
            case Some(h) => if(n == 1) h else getNthElementFromBeginning(n - 1, h.next)
        }
        def countHelper(mBehind: ListElement[T], curElem: ListElement[T]): ListElement[T] = {
            curElem.next match {
                case None => mBehind
                case Some(elem) => countHelper(mBehind.next.get, elem)
            }
        }
        val mthElemFromLast = headNode match {
            case None => throw new IllegalStateException("List is not long enough")
            case Some(h) => countHelper(h, getNthElementFromBeginning(m, headNode))
        }
        mthElemFromLast.data
    }
}

object LinkedList {
    def apply[T](elems: List[T]) = {
        val initial: LinkedList[T] = MutableLinkedList[T](None)
        (initial /: elems)((acc, e) => acc insertAtEnd e)
    }
}