package io.nsq

import java.net.InetSocketAddress


class Client(conf: Config) {


  def producer(address: InetSocketAddress): Producer = ???

  def consumer(topic: String, channel: String, address: InetSocketAddress): Consumer = ???


}
