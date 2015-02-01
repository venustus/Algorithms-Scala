package org.venustus.algorithms.basic

import org.scalatest.{FlatSpec, Matchers}

/**
 * Created by venkat on 01/02/15.
 */
class ACMICPCTeamSpec extends FlatSpec with Matchers {

    "ACMICPCTeam" should "return correct value" in {
        ACMICPCTeam.findMaxTopicPairs(List("10101", "11100", "11010", "00101")) should be ((5, 2))
    }

}
