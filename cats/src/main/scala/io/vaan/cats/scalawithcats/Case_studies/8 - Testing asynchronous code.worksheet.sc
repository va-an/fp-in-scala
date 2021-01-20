// how to test asynchronous as synchronous

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import cats.implicits._
import cats.{Id, Applicative}

val hostnames = List(
  "host1",
  "host21"
)

// interface
trait UptimeClient[F[_]] {
  def getUptime(hostname: String): F[Int]  
}

// impl for tests
case class TestUptimeClient(hosts: Map[String, Int]) extends UptimeClient[Id] {
  override def getUptime(hostname: String): Int = 
    hosts.getOrElse(hostname, 0)
}

// impl for production
case class RealUptimeClient() extends UptimeClient[Future] {
  override def getUptime(hostname: String): Future[Int] = 
    Future.successful(hostname.length)
}

// we can create service with any impl of UptimeClient 
case class UptimeService[F[_]: Applicative](client: UptimeClient[F]) {
  def getTotalUptime(hostnames: List[String]): F[Int] = 
    hostnames.traverse(client.getUptime).map(_.sum)
}

val testClient = TestUptimeClient(hosts = Map("host1" -> 10, "host2" -> 6))
testClient.getUptime("host2")

val testService = UptimeService(testClient)
testService.getTotalUptime(hostnames)

val realClient = RealUptimeClient()
realClient.getUptime("123")
realClient.getUptime("1234")

val realService = UptimeService(realClient)
realService.getTotalUptime(hostnames)

def testTotalUptime() = {
  val hosts    = Map("host1" -> 10, "host2" -> 6)
  val client   = new TestUptimeClient(hosts)
  val service  = new UptimeService(client)
  val actual   = service.getTotalUptime(hosts.keys.toList)
  val expected = hosts.values.sum
  assert(actual == expected)
}

testTotalUptime()