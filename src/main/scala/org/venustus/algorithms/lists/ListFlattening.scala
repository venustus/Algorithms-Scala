package org.venustus.algorithms.lists

/**
  * Created by venkat on 17/09/16.
  */
object ListFlattening {

    case class Node[T](data: T, var next: Option[Node[T]], child: Option[MyList[T]])

    case class MyList[T](head: Node[T], tail: Node[T])

    /**
      * Flattens a tree-like data structure in which each node has both a next pointer
      * and a child pointer.
      *
      * This algorithm traverses the tree while every time we encounter a non-empty child, we
      * append that child to the end of the current list, bringing it to the first level.
      *
      * Time complexity: O(n)
      *
      * @param l
      * @tparam T
      */
    def flattenList[T](l: MyList[T]) = {
        var curHead: Option[Node[T]] = Some(l.head)
        var curTail: Option[Node[T]] = Some(l.tail)
        while(curHead.isDefined) {
            if(curHead.get.child.isDefined) {
                curTail.get.next = Some(curHead.get.child.get.head)
                curTail = Some(curHead.get.child.get.tail)
            }
            curHead = curHead.get.next
        }
    }

}
