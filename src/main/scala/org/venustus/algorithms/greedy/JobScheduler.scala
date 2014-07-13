package org.venustus.algorithms.greedy

/**
 * Created by venkat on 12/07/14.
 */

case class Job(val weight: Long, val length: Long) {

}

class JobScheduler(jobs: Seq[Job]) {
    def ratioCompFunc(j1: Job, j2: Job) = {
        (j1.weight.toDouble / j1.length.toDouble) >= (j2.weight.toDouble / j2.length.toDouble)
    }
    def getSumOfWeightedCompletionTimes(compFunc: ((Job, Job) => Boolean) = ratioCompFunc): Long = {
        val orderedJobs = jobs.sortWith(compFunc)
        val res = orderedJobs.foldLeft((0L, 0L))((acc: (Long, Long), job: Job) =>
            (acc._1 + job.length, acc._2 + job.weight * (acc._1 + job.length)))
        res._2
    }
}

object JobSchedulerFactory {
    def apply(url: String): JobScheduler= {
        val lines = scala.io.Source.fromURL(url).getLines.toList.tail
        new JobScheduler(lines map ((s: String) => {
            val jobElems: Array[String] = s.split(' ')
            Job(jobElems(0).toLong, jobElems(1).toLong)
        }))
    }
}



