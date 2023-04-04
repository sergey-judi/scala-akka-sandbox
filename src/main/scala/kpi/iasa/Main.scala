package kpi.iasa

import akka.actor.{ActorRef, ActorSystem, Props}
import kpi.iasa.actor.{CityActor, FinishCommand}

object Main extends App {

  val width = 14000
  val height = 17000
  val passengersNumber = 470
  val vehiclesNumber = 50

  val actorSystem = ActorSystem("test-actor-system")
  val cityActor: ActorRef = actorSystem.actorOf(Props(CityActor(width, height, passengersNumber, vehiclesNumber)), "city-actor")

  Thread.sleep(2000)
  cityActor ! FinishCommand
  Thread.sleep(2000)
  actorSystem.stop(cityActor)

  actorSystem.terminate()

}
