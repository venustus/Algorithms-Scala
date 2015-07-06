package org.venustus.algorithms.dp

/**
 * Created by venkat on 17/06/15.
 */
object StockMaximize {

    /**
     * Given a list of projected stock prices for a company for n days,
     * returns maximum profit we can make, by following these rules:
     * You can either buy one share on a day or sell any number of shares you own
     * or do no transaction at all.
     *
     * @param stockPrices
     */
    def maximizeProfit(stockPrices: Vector[Long]) = {
        val maxToRight = (List[Long]() /: Range(stockPrices.size - 1, -1, -1))((acc, i) => {
            acc match {
                case l :: _ => math.max(stockPrices(i + 1), l) :: acc
                case Nil => List(stockPrices(i))
            }
        })
        val ans = ((0, 0L, 0L, maxToRight) /: stockPrices.zipWithIndex)((acc, priceWithIndex) => {
            acc match {
                case (openShares, openSharesSum, profit, maxList) => {
                    priceWithIndex match {
                        case (price, index) => {
                            if(price >= maxList.head) {
                                (0, 0L, (profit + openShares * price - openSharesSum).toLong, maxList.tail)
                            }
                            else (openShares + 1, openSharesSum + price, profit, maxList.tail)
                        }
                    }
                }
            }
        })
        ans._3
    }

}
