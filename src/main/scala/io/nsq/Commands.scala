package io.nsq

import akka.util.ByteString

import scala.concurrent.duration.Duration

trait Command {

  protected val name: String
  protected val params: Seq[ByteString] = Nil
  protected val body: Option[ByteString] = None

  override def toString(): String = {
    if (params.isEmpty) {
      name.toString()
    } else {
      s"$name: ${params.mkString(" ")}"
    }
  }

  def encode(): ByteString = {
    println(s"encode: $name")
    ByteString(name)
  }

}


case object MagicV2 extends Command {
  protected override val name: String = "  V2"
}

case class Identify(conf: Map[String, String]) extends Command {
  protected override val name: String = "IDENTIFY"
  protected override val body: Option[ByteString] = Some(ByteString(conf.mkString("")))
}

case class Auth(secret: String) extends Command {
  protected override val name: String = "AUTH"
  protected override val body: Option[ByteString] = Some(ByteString(secret))
}

case class Register(topic: String, channel: String) extends Command {
  protected override val name: String = "REGISTER"
  protected override val params: Seq[ByteString] = List(ByteString(topic), ByteString(channel))
}

case class UnRegister(topic: String, channel: String) extends Command {
  protected override val name: String = "UNREGISTER"
  protected override val params: Seq[ByteString] = List(ByteString(topic), ByteString(channel))
}

case object Ping extends Command {
  protected override val name: String = "PING"
}

case class Publish(topic: String, message: ByteString) extends Command {
  protected override val name: String = "PUB"
  protected override val params: Seq[ByteString] = List(ByteString(topic))
  protected override val body: Option[ByteString] = Some(message)
}

case class MultiPublish(topic: String, messages: Seq[ByteString]) extends Command {
  protected override val name: String = "MPUB"
  protected override val params: Seq[ByteString] = List(ByteString(topic))
  protected override val body: Option[ByteString] = Some(ByteString(messages.mkString("")))
}

case class Subscribe(topic: String, channel: String) extends Command {
  protected override val name: String = "SUB"
  protected override val params: Seq[ByteString] = List(ByteString(topic), ByteString(channel))
}

case class Ready(count: Int) extends Command {
  protected override val name: String = "RDY"
  protected override val params: Seq[ByteString] = List(ByteString(count))
}

case class Finish(id: String) extends Command {
  protected override val name: String = "FIN"
  protected override val params: Seq[ByteString] = List(ByteString(id))
}

case class Requeue(id: String, delay: Duration) extends Command {
  protected override val name: String = "REQ"
  protected override val params: Seq[ByteString] = List(ByteString(id), ByteString(delay.toMillis))
}

case class Touch(id: String) extends Command {
  protected override val name: String = "TOUCH"
  protected override val params: Seq[ByteString] = List(ByteString(id))
}

case object StartClose extends Command {
  protected override val name: String = "CLS"
}

case object Nop extends Command {
  protected override val name: String = "NOP"
}
