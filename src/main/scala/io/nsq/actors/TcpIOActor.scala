package io.nsq.actors


import java.net.InetSocketAddress

import akka.actor.{Props, Actor, ActorLogging, ActorRef}
import akka.io.Tcp
import akka.io.Tcp.{CommandFailed, Connect, Connected, Received, Register, _}
import akka.util.{ByteString, ByteStringBuilder}
import io.nsq.{ConnectionClosed, _}

object TcpIOActor {

  def props(remote: InetSocketAddress, listener: Option[ActorRef] = None) = Props(classOf[TcpIOActor], remote, listener)

}

class TcpIOActor(address: InetSocketAddress, listener: Option[ActorRef]) extends Actor with ActorLogging {

  import context._

  private var currAddress = address

  private val tcp = akka.io.IO(Tcp)(context.system)

  // todo watch tcpWorker
  private var tcpWorker: ActorRef = null

  private val operationActor = context.actorOf(OperationActor.props(self))

  private val bufferWrite: ByteStringBuilder = new ByteStringBuilder

  private var readyToWrite = false


  override def preStart() {
    if (tcpWorker != null) {
      tcpWorker ! Close
    }
    log.info(s"Connect to $currAddress")
    tcp ! Connect(currAddress)
  }

  def reconnect() = {
    become(receive)
    preStart()
  }

  override def postStop() {
    log.info("RedisWorkerIO stop")
  }

  def initConnectedBuffer() {
    readyToWrite = true
  }

  def receive = connecting orElse writing

  def connecting: Receive = {
    case a: InetSocketAddress => onAddressChanged(a)
    case c: Connected => onConnected(c)
    case Reconnect => reconnect()
    case c: CommandFailed => onConnectingCommandFailed(c)
    case c: ConnectionClosed => onClosingConnectionClosed() // not the current opening connection
  }

  def onConnected(cmd: Connected) = {
    sender ! Register(self)
    tcpWorker = sender
    initConnectedBuffer()
    tryInitialWrite() // TODO write something in head buffer
    become(connected)
    log.info("Connected to " + cmd.remoteAddress)
  }

  def onConnectingCommandFailed(cmdFailed: CommandFailed) = {
    log.error(cmdFailed.toString)
    scheduleReconnect()
  }

  def connected: Receive = writing orElse reading

  private def reading: Receive = {
    case WriteAck => tryWrite()
    case Received(dataByteString) => {
      if(sender == tcpWorker)
        operationActor ! dataByteString
      else
        onDataReceivedOnClosingConnection(dataByteString)
    }
    case a: InetSocketAddress => onAddressChanged(a)
    case c: ConnectionClosed => {
      if(sender == tcpWorker)
        onConnectionClosed(c)
      else
        onClosingConnectionClosed()
    }
    case c: CommandFailed => onConnectedCommandFailed(c)
    case HeartBeat => write(Nop)
    case data: Data => onDataReceived(data)
  }

  def onAddressChanged(addr: InetSocketAddress) {
    log.info(s"Address change [old=$address, new=$addr]")
    tcpWorker ! ConfirmedClose // close the sending direction of the connection (TCP FIN)
    currAddress = addr
    scheduleReconnect()
  }

  def onConnectionClosed(c: ConnectionClosed) = {
    log.warning(s"ConnectionClosed $c")
    scheduleReconnect()
  }

  /** O/S buffer was full
    * Maybe to much data in the Command ?
    */
  def onConnectedCommandFailed(commandFailed: CommandFailed) = {
    log.error(commandFailed.toString) // O/S buffer was full
    tcpWorker ! commandFailed.cmd
  }

  def scheduleReconnect() {
    cleanState()
    log.info(s"Trying to reconnect in $reconnectDuration")
    this.context.system.scheduler.scheduleOnce(reconnectDuration, self, Reconnect)
    become(receive)
  }

  def cleanState() {
    onConnectionClosed()
    readyToWrite = false
    bufferWrite.clear()
  }

  def writing: Receive = {
    case cmd: io.nsq.Command => write(cmd)
    case op: io.nsq.Operation =>
      operationActor ! op
      write(op.command)
  }

  private def onConnectionClosed() = operationActor ! ConnectionClosed

  private def onDataReceived(data: Data) = listener.map(_ ! data)

  private def onDataReceivedOnClosingConnection(dataByteString: ByteString) = operationActor ! dataByteString

  private def onClosingConnectionClosed() = operationActor ! ClosingConnectionClosed

  private def restartConnection() = reconnect()

  private def tryInitialWrite() {
    tryWrite()
  }

  def tryWrite() {
    if (bufferWrite.length == 0) {
      readyToWrite = true
    } else {
      writeWorker(bufferWrite.result())
      bufferWrite.clear()
    }
  }

  def write(cmd: io.nsq.Command) {
    val byteString: ByteString = cmd.encode()
    if (readyToWrite) {
      writeWorker(byteString)
    } else {
      bufferWrite.append(byteString)
    }
  }

  import scala.concurrent.duration.{DurationInt, FiniteDuration}

  def reconnectDuration: FiniteDuration = 2 seconds

  private def writeWorker(byteString: ByteString) {
    tcpWorker ! Write(byteString, WriteAck)
    readyToWrite = false
  }

}


object WriteAck extends Event

object Reconnect