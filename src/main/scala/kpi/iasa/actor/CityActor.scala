package kpi.iasa.actor

import akka.actor.{Actor, ActorRef, Cancellable, Props}
import kpi.iasa.model.{Passenger, Point, Trip, Vehicle}
import org.slf4j.{Logger, LoggerFactory}

import scala.collection.mutable
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.{Duration, DurationDouble}
import scala.util.Random

object CityActor {

  def apply(width: Int, height: Int, passengersNumber: Int, vehiclesNumber: Int): CityActor = {
    val passengers: List[Passenger] = initPassengers(width, height, passengersNumber)
    val vehicles: List[Vehicle] = initVehicles(width, height, vehiclesNumber)

    new CityActor(passengers, vehicles)
  }

  private def initPassengers(width: Int, height: Int, passengersNumber: Int) =
    List.tabulate(passengersNumber)(id => initPassenger(id, Random.nextInt(width), Random.nextInt(height)))

  private def initPassenger(id: Int, x: Int, y: Int) = Passenger(id, Point(x, y))

  private def initVehicles(width: Int, height: Int, vehiclesNumber: Int) =
    List.tabulate(vehiclesNumber)(id => initVehicle(id, Random.nextInt(width), Random.nextInt(height)))

  private def initVehicle(id: Int, x: Int, y: Int) = Vehicle(id, Point(x, y))

}

class CityActor(var passengers: List[Passenger], val vehicles: List[Vehicle]) extends Actor {

  implicit val executionContext: ExecutionContext = ExecutionContext.global

  val log: Logger = LoggerFactory.getLogger(classOf[CityActor])

  var scheduler: Cancellable = _
  val drivers: mutable.Map[String, ActorRef] = mutable.Map()
  val tripRegistry: ActorRef = context.actorOf(Props(TripRegistryActor()), "trip-registry-actor")

  var recordedTrips: List[Trip] = List()

  override def receive: Receive = {
    case FinishCommand =>
      scheduler.cancel()
      tripRegistry ! DisplayStatistics
    case TickCommand =>
      checkCitizens()

      tripCandidates.foreach {
        passenger =>
          val otherCitizen = randomCitizen

          if (passenger != otherCitizen && otherCitizen.isHospitable) {
            buildRoute(passenger, otherCitizen)
          } else {
            log.debug(s"NO TRIP FOR $passenger AND $otherCitizen")
          }
      }
    case _ => log.error("UNKNOWN: Received unknown command")
  }

  override def preStart(): Unit =
    this.scheduler = context.system.scheduler.scheduleWithFixedDelay(Duration.Zero, 1.second, self, TickCommand)

  private def tripCandidates: List[Passenger] = passengers.filter(_.isReadyForTrip)

  private def checkCitizens(): Unit = passengers.foreach(_.checkState())

  private def randomCitizen: Passenger = passengers(Random.nextInt(passengers.length))

  private def buildRoute(passenger: Passenger, host: Passenger): Unit = {
    findClosestVehicleAvailable(passenger) match {
      case Some(closestVehicle) =>
        closestVehicle.setBusy()

        passenger.state = Passenger.OnTrip
        host.state = Passenger.GuestAwaiting

        val driverActor = enrollDriver(closestVehicle)

        log.info(s"Building route using [$driverActor]")
        driverActor ! BuildRouteCommand(passenger, host, tripRegistry)
        case None => log.debug("No vehicles available")
    }
  }

  private def findClosestVehicleAvailable(passenger: Passenger): Option[Vehicle] =
    vehicles
      .sortBy(_.distanceTo(passenger.currentLocation))
      .find(_.state == Vehicle.Free)

  private def enrollDriver(vehicle: Vehicle): ActorRef = {
    val driverActorName = s"driver-actor-${vehicle.id}"

    drivers.get(driverActorName) match {
      case Some(driverActor) => driverActor
      case None =>
        val driverActor = context.actorOf(Props(DriverActor(vehicle)), driverActorName)
        drivers.put(driverActorName, driverActor)
        driverActor
    }
  }

}
