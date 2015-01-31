package org.venustus.algorithms.recursion

import org.scalacheck.Gen.oneOf

/**
 * Created by venkat on 08/01/15.
 */
object ParenthesisGenerator {

    def generateBalancedParens(n: Int) = {
        def genBalancedParensHelper(leftParens: Int, rightParens: Int): Set[String] = {
            if(leftParens == rightParens && leftParens == n) Set("")
            else if(leftParens == rightParens && leftParens < n)
                for {
                    str <- genBalancedParensHelper(leftParens + 1, rightParens)
                } yield "(" + str
            else if(leftParens > rightParens) {
                if(leftParens == n) for { str <- genBalancedParensHelper(leftParens, rightParens + 1) } yield ")" + str
                else for {
                        ch <- Set("(", ")")
                        newLp = if(ch == "(") leftParens + 1 else leftParens
                        newRp = if(ch == ")") rightParens + 1 else rightParens
                        str <- genBalancedParensHelper(newLp, newRp)

                    } yield ch + str
            }
            else Set()
        }
        genBalancedParensHelper(0, 0)
    }

}
