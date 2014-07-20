package org.venustus.algorithms.unionfind

import org.scalatest.{FlatSpec, Matchers}

/**
 * Created by venkat on 20/07/14.
 */
class UnionFindSpec extends FlatSpec with Matchers {
    val simpleSet: Set[Int] = Set[Int](1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    val unionFindDS: UnionFind[Int] = OptimizedUnionFind(simpleSet)

    "Simple union find data structure" should "return same values on find before any union" in {
        (unionFindDS find(1))._1 should be (1)
        (unionFindDS find(3))._1 should be (3)
        (unionFindDS find(5))._1 should be (5)
        (unionFindDS find(10))._1 should be (10)
    }

    it should "return correct leaders after some unions" in {
        val u0 = unionFindDS union (1, 2)
        val (r1, u1) = u0 find (2)
        r1 should be (2)
        val (r2: Int, u2: UnionFind[Int]) = u1 find (1)
        r2 should be (2)
        val u3 = u2 union (3, 4)
        val (r3, u4) = u3 find (3)
        r3 should be (4)
        val u5 = (u4 union (7, 8)) union (3, 7)
        val (r4, u6) = u5 find (3)
        r4 should be (8)
        val (r5, u7) = u6 find (4)
        r5 should be (8)
        val (r6, u8) = u7 find (7)
        r6 should be (8)
        val u9 = u8 union (5, 1)
        val (r7, u10) = u9 find (5)
        r7 should be (2)
        val u11 = u10 union (5, 3)
        val (r8, u12) = u11 find (5)
        r8 should be (8)
        val (r9, u13) = u12 find (2)
        r9 should be (8)
    }
}
