package kpi.iasa

package model {

  import kpi.iasa.model.Passenger.{AtHome, ChillingWithFriend, PassengerState}
  import kpi.iasa.model.Vehicle.{Busy, Free, VehicleState}

  case class Point(x: Int, y: Int) {
    def distanceTo(other: Point): Double =
      Math.sqrt(Math.pow(x - other.x, 2) + Math.pow(y - other.y, 2))
  }

  object Passenger {
    sealed trait PassengerState

    case object AtHome extends PassengerState

    case object TripAwaiting extends PassengerState

    case object GuestAwaiting extends PassengerState

    case object ChillingWithFriend extends PassengerState

    //    case object WillingToContact extends PassengerState

    //    case object WillingToGoHome extends PassengerState

    case object OnTrip extends PassengerState

    def apply(id: Int, home: Point) = new Passenger(id, home)

    def apply(id: Int, home: Point, state: PassengerState) = new Passenger(id, home, state)
  }

  case class Passenger(id: Int,
                       home: Point,
                       var currentLocation: Point,
                       var state: PassengerState = AtHome,
                       var lastUpdatedAt: Long = System.currentTimeMillis(),
                       var chillingTime: Long = 0) {
    def this(id: Int, home: Point) = this(id, home, home)

    def this(id: Int, home: Point, state: PassengerState) = this(id, home, home, state)

    def isReadyForTrip: Boolean =
      this.state == Passenger.AtHome || this.state == Passenger.TripAwaiting

    def isHospitable: Boolean =
      this.state == Passenger.AtHome

    def checkState(): Unit =
      if (this.state == ChillingWithFriend && isChilledEnough) {
        if (this.home == this.currentLocation) {
          this.state = Passenger.AtHome
        } else {
          this.state = Passenger.TripAwaiting
        }
      }

    def isChilledEnough: Boolean =
      System.currentTimeMillis() - this.lastUpdatedAt > this.chillingTime
  }

  object Vehicle {
    val defaultSpeed = 60

    sealed trait VehicleState

    case object Busy extends VehicleState

    case object Free extends VehicleState

    def apply(id: Int, currentLocation: Point) = new Vehicle(id, defaultSpeed, currentLocation)
  }

  case class Vehicle(id: Int, speed: Int = Vehicle.defaultSpeed, var currentLocation: Point, var state: VehicleState = Free) {
    def setBusy(): Unit = this.state = Busy

    def setFree(): Unit = this.state = Free

    def distanceTo(destination: Point): Double = this.currentLocation.distanceTo(destination)
  }

  case class Trip(passenger: Passenger, vehicle: Vehicle, from: Point, to: Point, timeAwaiting: Long, duration: Long)

  case class TripStatistics(averageTimeAwaiting: Long, averageTimeOnTrip: Long)

}
