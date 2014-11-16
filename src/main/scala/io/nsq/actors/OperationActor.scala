package io.nsq.actors

import akka.actor.{ActorRef, Actor, ActorLogging, Props}
import akka.util.ByteString
import io.nsq._

import scala.collection.mutable


object OperationActor {
  def props(tcpIo: ActorRef) = Props(classOf[OperationActor], tcpIo)
}

class OperationActor(tcpIo: ActorRef)  extends Actor with ActorLogging {

  private val queuePromises = mutable.Queue[Operation]()

  private val decoderActor = context.actorOf(DecoderActor.props())

  override def postStop() {
    queuePromises.foreach(op => {
      op.completeFailed(StoppingOperationActorException)
    })
  }

  override def receive: Receive = {
    case op: Operation => queuePromises += op
    case bs: ByteString => decoderActor ! bs
    case HeartBeat => tcpIo ! HeartBeat
    case data: Data => tcpIo ! data
    case res: SuccessResponse => queuePromises.dequeue().completeSuccess(res)
    case res: ErrorResponse => queuePromises.dequeue().completeFailed(res)
    case fail: ConnectionFailure =>
      queuePromises.foreach(_.completeFailed(fail))
      queuePromises.clear()
  }

}
