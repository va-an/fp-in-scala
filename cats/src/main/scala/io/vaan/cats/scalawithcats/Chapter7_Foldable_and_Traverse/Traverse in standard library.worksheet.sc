// traverse example with Futures

import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

val hostnames = List(
  "alpha.example.com",
  "beta.example.com",
  "gamma.demo.com"
)

def getUptime(hostname: String): Future[Int] =
  Future(hostname.length * 60)

// manually folding Futures
val allUptimes: Future[List[Int]] = 
  hostnames.foldLeft(Future(List.empty[Int])) { (a, i) =>
    val uptimeF = getUptime(i)
    for {
      acc <- a
      uptime <- uptimeF
    } yield acc :+ uptime
  }

Await.result(allUptimes, 1 second)

// with traverse
val allUptimesTraverse: Future[List[Int]] =
  Future.traverse(hostnames)(getUptime)

Await.result(allUptimesTraverse, 1 second)
