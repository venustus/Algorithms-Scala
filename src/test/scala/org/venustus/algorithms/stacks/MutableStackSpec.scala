package org.venustus.algorithms.stacks

import java.util.EmptyStackException

import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by venkat on 17/09/16.
  */
class MutableStackSpec extends FlatSpec with Matchers {

    "Standard push/pop operations" should "work as expected" in {
        val myStack = new MutableStack[Int]()

        an [EmptyStackException] should be thrownBy {
            myStack pop
        }

        myStack push 1
        myStack push 2
        myStack push 3

        (myStack top) should be (3)
        (myStack pop)._1 should be (3)
        (myStack top) should be (2)
    }

}
