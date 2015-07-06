package org.venustus.algorithms.search

import org.scalatest.{FlatSpec, Matchers}

/**
 * Created by venkat on 04/06/15.
 */
class GridSearchSpec extends FlatSpec with Matchers {
    "Algorithm to search for a sub-matrix in a bigger matrix" should "return correct matches" in {
        GridSearch.searchForGrid(Array("1234567890", "0987654321", "1111111111", "1111111111", "2222222222"),
            Array("876543", "111111", "111111")) should be (true)
        GridSearch.searchForGrid(Array("7283455864",
                                       "6731158619",
                                       "8988242643",
                                       "3830589324",
                                       "2229505813",
                                       "5633845374",
                                       "6473530293",
                                       "7053106601",
                                       "0834282956",
                                       "4607924137"), Array("9505", "3845", "3530")) should be (true)
        GridSearch.searchForGrid(Array("400453592126560",
                                       "114213133098692",
                                       "474386082879648",
                                       "522356951189169",
                                       "887109450487496",
                                       "252802633388782",
                                       "502771484966748",
                                       "075975207693780",
                                       "511799789562806",
                                       "404007454272504",
                                       "549043809916080",
                                       "962410809534811",
                                       "445893523733475",
                                       "768705303214174",
                                       "650629270887160"), Array("99", "99")) should be (false)
    }
}
