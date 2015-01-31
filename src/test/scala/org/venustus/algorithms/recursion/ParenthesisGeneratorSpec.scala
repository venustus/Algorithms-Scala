package org.venustus.algorithms.recursion

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._
import org.scalatest.prop.Checkers
import org.scalatest.{FlatSpec, Matchers}
import org.venustus.algorithms.dp.CatalanNumbers

import scala.util.Random

/**
 * Created by venkat on 08/01/15.
 */

class QuickCheckParens extends Properties("ParenthesisGenerator") {
    property("parens size") = forAll(choose(0, 12)) { a: Int => {
            println(s"Checking for int $a")
            ((ParenthesisGenerator generateBalancedParens (a)) size) == (CatalanNumbers getNthCatalanNumber (a))
        }
    }
}

class ParenthesisGeneratorSpec extends FlatSpec with Matchers with Checkers {
    "Parenthesis generator" should "generate correct set of parens strings for small numbers n" in {
        ParenthesisGenerator generateBalancedParens(0) should be (Set(""))
        ParenthesisGenerator generateBalancedParens(1) should be (Set("()"))
        ParenthesisGenerator generateBalancedParens(2) should be (Set("()()", "(())"))
        ParenthesisGenerator generateBalancedParens(3) should be (Set("()()()", "(())()", "()(())", "(()())", "((()))"))
    }

    it should "generate correct number of parenthesis for large integers" in {
        check(new QuickCheckParens)
    }
}


