package io.nsq

import java.net.InetSocketAddress

import io.nsq.actors.TcpIOActor

import collection.mutable.Stack
import org.scalatest._

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

class NsqConnectionSpec extends FlatSpec with Matchers {

  "Nsq Connection" should "connect init ok" in {

    implicit val akkaSystem = akka.actor.ActorSystem()

    val tcpIo = akkaSystem.actorOf(TcpIOActor.props(new InetSocketAddress("localhost", 4161)))
    val conn = NsqConnection(tcpIo, new Config)
    val f = conn.connect().map(println)
    Await.result(f, 3 seconds)
  }

  it should "throw NoSuchElementException if an empty stack is popped" in {
    val emptyStack = new Stack[Int]
    a [NoSuchElementException] should be thrownBy {
      emptyStack.pop()
    }
  }
}