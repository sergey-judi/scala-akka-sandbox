package kpi.iasa.actor

import akka.actor.{Actor, Cancellable}
import kpi.iasa.model.{Passenger, Point, Trip, Vehicle}
import org.slf4j.{Logger, LoggerFactory}

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.{DurationLong, FiniteDuration}
import scala.util.Random

object DriverActor {

  def apply(vehicle: Vehicle) = new DriverActor(vehicle)

}

class DriverActor(vehicle: Vehicle) extends Actor {

  implicit val executionContext: ExecutionContext = ExecutionContext.global

  val log: Logger = LoggerFactory.getLogger(classOf[DriverActor])
  val speed = 60

  override def receive: Receive = {
    case BuildRouteCommand(passenger, host, replyTo) =>
      log.info(s"BuildRouteCommand : $passenger -> $host & $vehicle")

      val startLocation = passenger.currentLocation
      val destination = host.home

      val timeToAwaitMillis = timeToArrive(passenger.currentLocation)
      val tripDurationMillis = timeToTrip(passenger.currentLocation, host.home)

      val totalDuration = timeToAwaitMillis + tripDurationMillis

      scheduleReply(totalDuration.nanoseconds) {
        vehicle.currentLocation = destination
        vehicle.setFree()

        passenger.state = Passenger.ChillingWithFriend
        host.state = Passenger.ChillingWithFriend

        val currentTime = System.currentTimeMillis()
        val chillingTime = Random.nextInt(1000)

        passenger.currentLocation = destination
        passenger.lastUpdatedAt = currentTime
        host.lastUpdatedAt = currentTime

        passenger.chillingTime = chillingTime
        host.chillingTime = chillingTime

        log.debug(s"BuildRouteCommand : releasing $vehicle")
        val trip = Trip(passenger, vehicle, startLocation, destination, timeToAwaitMillis, tripDurationMillis)
        replyTo ! RegisterTripCommand(trip)
      }
    case _ => log.error("UNKNOWN: received unknown command")
  }

  def timeToArrive(pickupPoint: Point): Long =
    (vehicle.currentLocation.distanceTo(pickupPoint) / this.speed * 3600 * 1000).toLong

  def timeToTrip(start: Point, finish: Point): Long =
    (start.distanceTo(finish) / this.speed * 3600 * 1000).toLong

  def scheduleReply(duration: FiniteDuration)(f: => Unit): Cancellable =
    context.system.scheduler.scheduleOnce(duration)(f)

}
