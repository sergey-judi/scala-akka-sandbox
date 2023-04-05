package kpi.iasa.actor

import akka.actor.Actor
import kpi.iasa.model.{Trip, TripStatistics}
import org.slf4j.{Logger, LoggerFactory}

object TripRegistryActor {

  def apply() = new TripRegistryActor

}

class TripRegistryActor extends Actor {

  val log: Logger = LoggerFactory.getLogger(classOf[TripRegistryActor])

  var recordedTrips: List[Trip] = List()
  var unsuccessfulTrips: List[Trip] = List()

  override def receive: Receive = {
    case DisplayStatistics =>
      log.info("---- SUCCESSFUL TRIPS RECORDED ----")
      logStatistics(recordedTrips)

      log.info("---- UNSUCCESSFUL TRIPS RECORDED ----")
      logStatistics(unsuccessfulTrips)
    case RegisterUnsuccessfulTripCommand(trip) =>
      log.info(s"RegisterUnsuccessfulTripCommand : Passenger#${trip.passenger.id} is unsatisfied with Vehicle#${trip.vehicle.id}")
      unsuccessfulTrips = unsuccessfulTrips :+ trip
    case RegisterTripCommand(trip) =>
      log.info(s"RegisterTripCommand : Passenger#${trip.passenger.id} arrived on Vehicle#${trip.vehicle.id}")
      recordedTrips = recordedTrips :+ trip
    case _ => log.error("UNKNOWN: received unknown command")
  }

  private def logStatistics(trips: List[Trip]) = {
    trips.groupBy(_.passenger.id)
      .map {
        case (passengerId, passengerTrips) => (passengerId, averageStatistics(passengerTrips))
      }
      .foreach {
        case (passengerId, tripsStatistics: TripStatistics) =>
          log.info(
            s"Passenger#$passengerId "
              + s"| Avg time awaiting = ${tripsStatistics.averageTimeAwaiting} "
              + s"| Avg time on trip = ${tripsStatistics.averageTimeOnTrip} "
              + s"| Avg trip cost = ${tripsStatistics.averageTripCost}"
          )
      }
  }

  private def averageStatistics(trips: List[Trip]): TripStatistics = {
    val averageTimeAwaiting = trips.map(_.timeAwaiting).sum / trips.size
    val averageTimeOnTrip = trips.map(_.duration).sum / trips.size
    val averageTripCost = trips.map(_.cost).sum / trips.size

    TripStatistics(averageTimeAwaiting, averageTimeOnTrip, averageTripCost)
  }

}
