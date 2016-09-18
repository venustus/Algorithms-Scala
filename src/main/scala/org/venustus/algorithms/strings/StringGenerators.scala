package org.venustus.algorithms.strings

/**
  * Created by venkat on 17/09/16.
  */
object StringGenerators {

    def generatePermutation(str: String): Set[String] = {
        def permuteHelper(charArr: Array[Char]): Set[String] = {
            (Set[String]() /: charArr.indices) ((acc, i) => {
                if(charArr.length == 1) Set(charArr(i).toString)
                else {
                    acc ++ (
                        for {
                            rest <- permuteHelper(charArr.slice(0, i) ++ charArr.slice(i + 1, charArr.length))
                        } yield charArr(i).toString + rest
                    )
                }
            })
        }
        permuteHelper(str.toCharArray)
    }

    def generateCombinations(str: String): Set[String] = {
        def combineHelper(charArr: Array[Char]): Set[String] = {
            (Set[String]() /: charArr.indices) ((acc, i) => {
                if(charArr.length == 1) Set(charArr(i).toString, "")
                else {
                    acc ++ (
                        for {
                            prefix <- Set(charArr(i).toString, "")
                            rest <- combineHelper(charArr.slice(i + 1, charArr.length))
                        } yield prefix + rest
                    )
                }
            })
        }
        combineHelper(str.toCharArray) - ""
    }

    /**
      * Prints all possible translations of phone numbers into alphabets as written
      * on phone keypads.
      * @param phoneNumber
      */
    def printWordsForPhoneNumber(phoneNumber: Array[Int]) = {
        def getCharKey(i: Int, place: Int): Char = {
            val phoneKeyLetters = List('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y')
            phoneKeyLetters((i - 2) * 3 + place)
        }
        def printWords(prefix: StringBuilder, start: Int): Unit = {
            val possibilitiesForCurPosition = phoneNumber(start) match {
                case 0 => List('0')
                case 1 => List('1')
                case y => List(0, 1, 2) map ((place) => getCharKey(y, place))
            }
            possibilitiesForCurPosition foreach ((ch) => {
                prefix.append(ch)
                if(start == phoneNumber.length - 1) println(prefix.toString)
                else {
                    printWords(prefix, start + 1)
                }
                prefix.setLength(prefix.length - 1)
            })
        }
        printWords(new StringBuilder, 0)
    }

}
