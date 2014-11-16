package io.nsq

import scala.concurrent.Promise

case class Operation(command: Command, promise: Promise[Response]) {

  def completeSuccess(response: Response): Option[Response] = {
    promise.success(response)
    None
  }

  def completeFailed(t: Throwable) = promise.failure(t)

}
