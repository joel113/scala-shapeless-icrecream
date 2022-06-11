object ShapelessOps {

  import shapeless._
  import shapeless.ops.hlist

  // 1
  implicit class AlignerOps[ARepr <: HList](a: ARepr) {
    def as[B](implicit aligner: Aligner[ARepr, B]): B = aligner.apply(a)
  }

  // 2
  trait Aligner[A, B] {
    def apply(a: A): B
  }

  // 3
  implicit def genericAligner[ARepr <: HList, B, BRepr <: HList](
    implicit
    bGen: LabelledGeneric.Aux[B, BRepr],
    align: hlist.Align[ARepr, BRepr]
  ): Aligner[ARepr, B] = new Aligner[ARepr, B] {
    def apply(a: ARepr): B = bGen.from(align.apply(a))
  }

}

object Vehicles {

  import java.time.LocalDate
  import shapeless.LabelledGeneric
  import shapeless.record._
  import shapeless.syntax.singleton._

  case class Location(pickup: String, dropOff: String, to: LocalDate, from: LocalDate)

  case class Vehicle(vehicleCategory: String, automatic: Boolean, numDoors: Int, isDiesel: Boolean)

  case class Reservation(
    pickup: String,
    dropOff: String,
    from: LocalDate,
    to: LocalDate,
    vehicleCategory: String,
    automatic: Boolean,
    numDoors: Int,
    driverAge: Int,
    nationality: String,
    confirmed: Boolean // this is new
  )

  case class Driver(driverAge: Int, nationality: String)

  def main(args: Array[String]): Unit = {
    import ShapelessOps._

    // Our case class representations of the data
    // See the updated case class declaration for Location
    val location = Location(pickup = "Malaga Airport", dropOff = "Malaga Airport", from = LocalDate.of(2018, 8, 1), to = LocalDate.of(2018, 8, 10))
    val vehicle = Vehicle(vehicleCategory = "Economy", automatic = false, numDoors = 4, isDiesel = true)
    val driver = Driver(driverAge = 35, nationality = "British")

    val LocationGen = LabelledGeneric[Location]
    val VehicleGen = LabelledGeneric[Vehicle]
    val DriverGen = LabelledGeneric[Driver]
    val ReservationGen = LabelledGeneric[Reservation]

    val locationRepr = LocationGen.to(location)
    val (_, vehicleRepr) = VehicleGen.to(vehicle).remove('isDiesel)
    val driverRepr = DriverGen.to(driver)

    val misalignedReservationRepr = locationRepr.map(_) ++ vehicleRepr ++ driverRepr :+ ('confirmed ->> true)

    // closer!
    /*_*/ val reservation: Reservation = misalignedReservationRepr.as[Reservation] /*_*/
    println(s"from date: ${reservation.from}")
  }
}