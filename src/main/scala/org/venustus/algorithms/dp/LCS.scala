package org.venustus.algorithms.dp

import scala.collection.immutable.Queue

/**
 * Created by venkat on 03/04/15.
 */
object LCS {

    /**
     * Returns longest common subsequence of two sequences.
     *
     * Approach: Dynamic Programming
     *
     * The basic recursive relation is:
     * len[i, j] = if(seq1[i] == seq2[j]) max(len[i - 1, j - 1] + 1, len[i - 1, j], len[i, j - 1])
     *             else max(len[i - 1, j], len[i, j - 1])
     * Basically we store a two-dimensional array tracking indexes of the two arrays respectively. The eventual answer
     * shall be in len[m - 1, n - 1].
     * Base cases: len[0, 0] = 1 if seq1[0] = seq2[0] or 0 otherwise
     *
     * Time complexity: O(m * n) where m is size of first sequence and n is the size of second sequence
     * @param seq1
     * @param seq2
     * @tparam T
     */
    def findLongestCommonSubsequence[T](seq1: IndexedSeq[T], seq2: IndexedSeq[T]): Seq[T] = {
        val m = seq1.size
        val n = seq2.size
        val tableWithAllFirstColumnsZero = (Vector.fill[Int](m + 1, n + 1)(0) /: (0 until m + 1))((acc, i) => acc updated (i, acc(i) updated (0, 0)))
        val table = (tableWithAllFirstColumnsZero /: (0 until n + 1))((acc, j) => acc updated (0, acc(0) updated (j, 0)))
        val filledTable = (table /: (1 to m))((tableAcc, rowNum) => {
            tableAcc updated (rowNum, (tableAcc(rowNum) /: (1 to n))((rowAcc, colNum) => {
                val lastCharMatch = seq1(rowNum - 1) == seq2(colNum - 1)
                val newVal =
                    if(lastCharMatch) tableAcc(rowNum - 1)(colNum - 1) + 1
                    else math.max(tableAcc(rowNum - 1)(colNum), rowAcc(colNum - 1))
                rowAcc updated (colNum, newVal)
            }))
        })
        def getLCS(i: Int, j: Int, ans: List[T]): List[T] = {
            if(i == 0 || j == 0) ans
            else {
                if(seq1(i - 1) == seq2(j - 1)) getLCS(i - 1, j - 1, seq1(i - 1) :: ans)
                else {
                    if(filledTable(i - 1)(j) > filledTable(i)(j - 1)) getLCS(i - 1, j, ans)
                    else getLCS(i, j - 1, ans)
                }
            }
        }
        getLCS(m, n, List[T]())
    }
}
