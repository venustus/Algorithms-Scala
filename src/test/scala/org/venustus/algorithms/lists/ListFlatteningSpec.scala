package org.venustus.algorithms.lists

import org.scalatest.{FlatSpec, Matchers}
import org.venustus.algorithms.lists.ListFlattening.{MyList, Node}

/**
  * Created by venkat on 17/09/16.
  */
class ListFlatteningSpec extends FlatSpec with Matchers {

    "List flattening algorithm" should "flatten the list correctly" in {
        val node7 = Node(7, None, None)
        val node1 = Node(1, None, None)
        val node6 = Node(6, None, None)
        val myHead = Node(5, Some(Node(33, Some(Node(17, Some(Node(2, Some(node1), Some(MyList(Node(2, Some(node7), None), node7)))), None)), None)),
                             Some(MyList(Node(6, Some(Node(25, Some(node6), None)), None), node6)))
        val myTail = node1

        val myList = MyList(myHead, myTail)
        ListFlattening.flattenList(myList)
        var curHead: Option[Node[Int]] = Some(myList.head)
        while(curHead.isDefined) {
            println(curHead.get.data)
            curHead = curHead.get.next
        }
    }

}
