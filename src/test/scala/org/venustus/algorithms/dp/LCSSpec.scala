package org.venustus.algorithms.dp

import org.scalatest.{FlatSpec, Matchers}

/**
 * Created by venkat on 12/07/15.
 */
class LCSSpec extends FlatSpec with Matchers {

    "Algorithm to retrieve longest common subsequence" should "work" in {
        LCS.findLongestCommonSubsequence[Char]("ABCDGH".toCharArray, "AEDFHR".toCharArray) should be (Seq[Char]('A', 'D', 'H'))
        LCS.findLongestCommonSubsequence[Char]("AGGTAB".toCharArray, "GXTXAYB".toCharArray) should be (Seq[Char]('G', 'T', 'A', 'B'))
        LCS.findLongestCommonSubsequence[Int](Vector(1, 2, 3, 4, 1), Vector(3, 4, 1, 2, 1, 3)) should be(Seq[Int](3, 4, 1))
        LCS.findLongestCommonSubsequence[Int](Vector(3, 9, 8, 3, 9, 7, 9, 7, 0), Vector(3, 3, 9, 9, 9, 1, 7, 2, 0, 6)) should be (Seq[Int](3, 3, 9, 9, 7, 0))
    }

}
