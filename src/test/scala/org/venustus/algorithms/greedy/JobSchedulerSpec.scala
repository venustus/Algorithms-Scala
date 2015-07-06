package org.venustus.algorithms.greedy

import org.scalatest.{Matchers, FlatSpec}

/**
 * Created by venkat on 12/07/14.
 */
class JobSchedulerSpec extends FlatSpec with Matchers {
    def differenceCompFunc(j1: Job, j2: Job) = {
        if(j1.weight - j1.length > j2.weight - j2.length) true
        else if(j1.weight - j1.length == j2.weight - j2.length) j1.weight > j2.weight
        else false
    }
    def ratioCompFunc(j1: Job, j2: Job) = {
        (j1.weight.toDouble / j1.length.toDouble) >= (j2.weight.toDouble / j2.length.toDouble)
    }
    val scheduler = JobSchedulerFactory("file:///Users/venkat/Documents/Projects/Algorithms/src/test/resources/org/venustus/algorithms/greedy/jobs.txt")

    "Testing job scheduler" should "return correct sum of weighted completion times" in {
        val scheduler2 = new JobScheduler(List(Job(1, 2), Job(2, 3), Job(3, 1)))
        scheduler2.getSumOfWeightedCompletionTimes(differenceCompFunc) should be (17)

        val scheduler3 = new JobScheduler(List(Job(3, 5), Job(1, 2)))
        scheduler3.getSumOfWeightedCompletionTimes(differenceCompFunc) should be (23)
        scheduler3.getSumOfWeightedCompletionTimes(ratioCompFunc) should be (22)
        scheduler3.getSumOfWeightedCompletionTimes() should be (22)

        println("Sub-optimal scheduler: " + scheduler.getSumOfWeightedCompletionTimes(differenceCompFunc))
    }

    "An optimal scheduler using ratio as optimization function" should "return optimal schedule" in {
        println("Optimal scheduler: " + scheduler.getSumOfWeightedCompletionTimes(ratioCompFunc))
    }
}
