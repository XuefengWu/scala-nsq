package io.nsq.actors

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import io.nsq.Data

import scala.collection.mutable
import scala.concurrent.Future
import scala.util.Try
import scala.concurrent.ExecutionContext.Implicits.global

object ConsumerActor {

  def props() = Props(classOf[ConsumerActor])

}

class ConsumerActor() extends Actor with ActorLogging {
  private val handlerFunctions: mutable.Set[Function[Data, Unit]] = mutable.HashSet[Function[Data, Unit]]()
  private val handlerActors: mutable.Set[ActorRef] = mutable.HashSet[ActorRef]()

  override def receive: Receive = {
    case handler: ActorRef => handlerActors += handler
    case handler: Function[Data, Unit] => handlerFunctions += handler
    case data: Data =>
      handlerActors.foreach(_ ! data)
      handlerFunctions.foreach(f => Future(Try(f(data))))
  }
}
