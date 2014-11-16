package io.nsq

import java.net.InetSocketAddress

import akka.actor.{ActorRef, ActorRefFactory}
import akka.util.ByteString
import io.nsq.actors.{ConsumerActor, TcpIOActor}

import scala.concurrent.{ExecutionContext, Future, Promise}


case class NsqConnection(tcpIo: ActorRef, conf: Config)(implicit ec: ExecutionContext) {

  def send(command: Command): Future[Response] = {
    val promise = Promise[Response]()
    tcpIo ! Operation(command, promise)
    promise.future
  }

  def connect(): Future[Response] = for {
    magicRes <- send(MagicV2)
    identifyRes <- magicRes.map(_ => send(Identify(Map("client_id" -> "client_idXX"))))
    authRes <- identifyRes.map(_ => send(Auth("")))
  } yield authRes

}

class Producer(address: InetSocketAddress,conf: Config)(implicit system: ActorRefFactory, ec: ExecutionContext)  {

  private val connection = NsqConnection(system.actorOf(TcpIOActor.props(address)), conf)

  def connect(): Future[Response] = connection.connect()

  def publish(topic: String, body: String*): Future[Response] = {
    if(body.length > 1) {
      connection.send(MultiPublish(topic, body.toSeq.map(v => ByteString(v))))
    } else {
      connection.send(Publish(topic, ByteString(body.mkString)))
    }
  }

}


class Consumer(topic: String, channel: String, address: Seq[InetSocketAddress], conf: Config)(implicit system: ActorRefFactory, ec: ExecutionContext)   {

  private val consumerActor: ActorRef = system.actorOf(ConsumerActor.props())
  private val connection = NsqConnection(system.actorOf(TcpIOActor.props(address.head, Some(consumerActor))), conf)

  def connect(): Future[Response] = connection.connect().flatMap(_ => connection.send(Subscribe(topic, channel)))

  def updateRDY(count: Int): Future[Response] = connection.send(Ready(count))

  def addHandler(func: Data => Unit): Unit = consumerActor ! func

  def addHandler(handler: ActorRef): Unit = consumerActor ! handler

}