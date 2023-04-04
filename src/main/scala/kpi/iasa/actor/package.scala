package kpi.iasa

package actor {

  import akka.actor.ActorRef
  import kpi.iasa.model.{Passenger, Trip}

  sealed trait Command

  case object TickCommand extends Command

  case object FinishCommand extends Command

  case object DisplayStatistics extends Command

  case class BuildRouteCommand(passenger: Passenger, host: Passenger, replyTo: ActorRef) extends Command

  case class RegisterTripCommand(trip: Trip) extends Command

}
