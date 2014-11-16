package io.nsq.actors

import akka.actor.{Actor, Props}
import akka.util.ByteString
import io.nsq.{AuthResponse, Response}


object DecoderActor {
  def props() = Props(classOf[DecoderActor])
}

class DecoderActor extends Actor {

  override def receive: Receive = {
    case bs: ByteString => sender() ! decode(bs)
  }

  def decode(bs: ByteString): Response =  AuthResponse("""{"identity":"01", "identity_url":"io.nsq", "permission_count":1}""")

}
