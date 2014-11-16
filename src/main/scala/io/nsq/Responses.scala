package io.nsq

import scala.concurrent.Future

import scala.concurrent.ExecutionContext.Implicits.global

case object NoConnectionException extends RuntimeException("No Connection established")

case object ClosedConnectionException extends RuntimeException("Closed Connection")

case object StoppingOperationActorException extends RuntimeException("Operation actor is stopping")


case class Message(time: java.util.Date, id: String, body: Array[Byte])

object FrameType extends Enumeration {
  type FrameType = Value
  val FrameTypeResponse = Value(0)
  val FrameTypeError = Value(1)
  val FrameTypeMessage = Value(2)
}

trait Response {
  def map(f: Response => Future[Response]): Future[Response]
}

abstract class ErrorResponse(msg: String) extends RuntimeException(msg) with Response {
  override def map(f: (Response) => Future[Response]): Future[Response] = Future(this)
}

abstract class ConnectionFailure(msg: String) extends RuntimeException(msg)

trait SuccessResponse extends Response {
  override def map(f: (Response) => Future[Response]): Future[Response] = f(this)
}


case object Invalid extends ErrorResponse("Invalid")

case object BadTopic extends ErrorResponse("BadTopic")

case object BadChannel extends ErrorResponse("BadChannel")

case object BadMessage extends ErrorResponse("BadMessage")

case object BadFailed extends ErrorResponse("BadFailed")


case object OkResponse extends SuccessResponse

case object HeartBeat extends Response {
  override def map(f: (Response) => Future[Response]): Future[Response] = f(this)
}

case class AuthResponse(identity: String) extends SuccessResponse

case class Data(size: Int, frameType: FrameType.FrameType, message: Message) extends SuccessResponse

case class IdentifyResponse(maxRdyCount: Int, tlsV1: Boolean, deflate: Boolean, snappy: Boolean, authRequired: Boolean) extends SuccessResponse

case object ConnectionClosed extends ConnectionFailure("ConnectionClosed")

case object ClosingConnectionClosed extends ConnectionFailure("ConnectionClosed")
