package demo

import java.net.InetSocketAddress

import akka.actor._
import akka.io.Tcp

object EchoConnectionHandler {
  def props(remote: InetSocketAddress, connection: ActorRef): Props =
    Props(new EchoConnectionHandler(remote, connection))
}

class EchoConnectionHandler(remote: InetSocketAddress, connection: ActorRef) extends Actor with ActorLogging {

  // We need to know when the connection dies without sending a `Tcp.ConnectionClosed`
  context.watch(connection)

  def receive: Receive = {
    case Tcp.Received(data) =>
      val text = data.utf8String.trim
      log.debug("Received '{}' from remote address {}", text, remote)
      text match {
        case "close" => context.stop(self)
        case _       => sender ! Tcp.Write(data)
      }
    case _: Tcp.ConnectionClosed =>
      log.debug("Stopping, because connection for remote address {} closed", remote)
      context.stop(self)
    case Terminated(`connection`) =>
      log.debug("Stopping, because connection for remote address {} died", remote)
      context.stop(self)
  }

}