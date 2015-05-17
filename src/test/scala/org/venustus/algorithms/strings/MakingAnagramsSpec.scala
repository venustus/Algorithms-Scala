package org.venustus.algorithms.strings

import org.scalatest.{Matchers, FlatSpec}

/**
 * Created by venkat on 14/05/15.
 */
class MakingAnagramsSpec extends FlatSpec with Matchers {
    "Making anagrams function" should "return correct number of minimum character deletions" in {
        MakingAnagrams.makeAnagram("fcrxzwscanmligyxyvym", "jxwtrhvujlmrpdoqbisbwhmgpmeoke") should be (30)
    }

    "Making anagrams by replacing function" should "return correct number of minimum character changes" in {
        MakingAnagrams.makeAnagram2("aaabbb") should be (Some(3))
        MakingAnagrams.makeAnagram2("ab") should be (Some(1))
        MakingAnagrams.makeAnagram2("abc") should be (None)
        MakingAnagrams.makeAnagram2("mnop") should be (Some(2))
        MakingAnagrams.makeAnagram2("xyyx") should be (Some(0))
        MakingAnagrams.makeAnagram2("xaxbbbxx") should be (Some(1))
        MakingAnagrams.makeAnagram2("hhpddlnnsjfoyxpciioigvjqzfbpllssuj") should be (Some(10))
        MakingAnagrams.makeAnagram2("xulkowreuowzxgnhmiqekxhzistdocbnyozmnqthhpievvlj") should be (Some(13))
        MakingAnagrams.makeAnagram2("aujteqimwfkjoqodgqaxbrkrwykpmuimqtgulojjwtukjiqrasqejbvfbixnchzsahpnyayutsgecwvcqngzoehrmeeqlgknnb") should be (Some(26))
        MakingAnagrams.makeAnagram2("dnqaurlplofnrtmh") should be (Some(5))
        MakingAnagrams.makeAnagram2("lbafwuoawkxydlfcbjjtxpzpchzrvbtievqbpedlqbktorypcjkzzkodrpvosqzxmpad") should be (Some(15))
        MakingAnagrams.makeAnagram2("drngbjuuhmwqwxrinxccsqxkpwygwcdbtriwaesjsobrntzaqbe") should be (None)
        MakingAnagrams.makeAnagram2("ubulzt") should be (Some(3))
        MakingAnagrams.makeAnagram2("gqdvlchavotcykafyjzbbgmnlajiqlnwctrnvznspiwquxxsiwuldizqkkaawpyyisnftdzklwagv") should be (None)
        MakingAnagrams.makeAnagram2("xtnipeqhxvafqaggqoanvwkmthtfirwhmjrbphlmeluvoa") should be (Some(13))
    }
}
