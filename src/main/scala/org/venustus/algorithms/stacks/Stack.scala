package org.venustus.algorithms.stacks

import java.util.EmptyStackException

import org.venustus.algorithms.lists.{LinkedList, MutableLinkedList}

/**
  * Created by venkat on 17/09/16.
  *
  * An interface for a stack of arbitrary type (either mutable or immutable)
  */
trait Stack[T] {
    def push(data: T): Stack[T]

    def pop: (T, Stack[T])

    def top: T
}

class MutableStack[T] extends Stack[T] {
    // start with an empty list
    private var list: LinkedList[T] = MutableLinkedList[T](None)

    def top: T = {
        try {
            list.getHead.data
        } catch {
            case ex: IllegalStateException => throw new EmptyStackException
        }
    }

    def push(data: T): Stack[T] = {
        list = list insertInFront data
        this
    }

    def pop = {
        try {
            val t = list.getHead
            list = list deleteElem t
            (t.data, this)
        } catch {
            case ex: IllegalStateException => throw new EmptyStackException
        }
    }
}
