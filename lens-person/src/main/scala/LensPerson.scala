import monocle.Lens
import monocle.macros.GenLens

import java.time.LocalDate

// https://www.optics.dev/Monocle/docs/optics/lens
object LensPerson:
  case class Person(_name: Name, _born: Born, _address: Address)

  case class Name(_foreNames: String /*Space separated*/, _surName: String)

  // Value of java.time.LocalDate.toEpochDay
  private type EpochDay = Long

  case class Born(_bornAt: Address, _bornOn: EpochDay)

  case class Address(_street: String, _houseNumber: Int, _place: String /*Village / city*/, _country: String)

  // Valid values of Gregorian are those for which 'java.time.LocalDate.of'
  // returns a valid LocalDate.
  case class Gregorian(_year: Int, _month: Int, _dayOfMonth: Int)

  // Implement these.
  private val street: Lens[Address, String]    = GenLens[Address](_._street)
  private val bornAddress: Lens[Born, Address] = GenLens[Born](_._bornAt)

  val bornStreet: Born => String = bornAddress.andThen(street).get

  private val setStreet: Lens[Person, Address] => (String => String) => (Person => Person) =
    l => f => l.andThen(street).modify(f)

  private val currentAddress: Lens[Person, Address] = GenLens[Person](_._address)

  val setCurrentStreet: String => Person => Person = s => setStreet(currentAddress)(_ => s)

  private val bornDate: Lens[Born, EpochDay] = GenLens[Born](_._bornOn)
  private val born: Lens[Person, Born]       = GenLens[Person](_._born)

  val setBirthMonth: Int => Person => Person = m =>
    born.andThen(bornDate).modify(epochToLocalDate.andThen(_.withMonth(m)).andThen(localDateToEpoch))

  // Transform both birth and current street names.
  val renameStreets: (String => String) => (Person => Person) = f =>
    setStreet(born.andThen(bornAddress))(f).andThen(setStreet(currentAddress)(f))

  private def epochToLocalDate(epochDays: Long): LocalDate =
    LocalDate.ofEpochDay(epochDays)

  private def localDateToEpoch(dt: LocalDate): Long = dt.toEpochDay
