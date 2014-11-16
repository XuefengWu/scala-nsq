package demo

import akka.actor.Actor
import akka.actor.ActorSystem
import akka.actor.Props

case class Greeting(who: String)

class GreetingActor extends Actor {
  def receive = {
    case Greeting(who) => println("Hello " + who)
  }
}

object Greeting extends App {

  val system = ActorSystem("MySystem")
  val greeter = system.actorOf(Props[GreetingActor], name = "greeter")
  greeter ! Greeting("Charlie Parker")

}