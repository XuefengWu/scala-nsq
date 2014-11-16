package demo

import java.net.InetSocketAddress

import akka.actor.{ActorSystem, Props, Actor, ActorLogging}
import akka.io.{IO, Tcp}

object EchoService {
  def props(endpoint: InetSocketAddress, system: ActorSystem): Props =
    Props(new EchoService(endpoint, system))
}

class EchoService(endpoint: InetSocketAddress, system: ActorSystem) extends Actor with ActorLogging {
  IO(Tcp)(system) ! Tcp.Bind(self, endpoint)
  override def receive: Receive = {
    case Tcp.Connected(remote, _) =>
      log.debug("Remote address {} connected", remote)
      sender ! Tcp.Register(context.actorOf(EchoConnectionHandler.props(remote, sender)))
  }
}
