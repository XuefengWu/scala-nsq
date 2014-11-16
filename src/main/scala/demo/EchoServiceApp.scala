package demo

import java.net.InetSocketAddress
import akka.actor.ActorSystem


object EchoServiceApp extends App {

  val system = ActorSystem("echo-service-system")
  val endpoint = new InetSocketAddress("localhost", 4161)
  system.actorOf(EchoService.props(endpoint, system), "echo-service")

  scala.io.StdIn.readLine(s"Hit ENTER to exit ...${System.getProperty("line.separator")}")
  system.shutdown()

}
