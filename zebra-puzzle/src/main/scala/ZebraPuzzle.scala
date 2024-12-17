import scala.reflect.Enum

enum Resident:
  case Englishman, Spaniard, Ukrainian, Norwegian, Japanese

private enum Color:
  case Red, Green, Ivory, Yellow, Blue

private enum Pet:
  case Dog, Fox, Horse, Snails, Zebra

private enum Drink:
  case Tea, Coffee, Milk, OrangeJuice, Water

private enum Smoke:
  case OldGold, Kools, Chesterfields, LuckyStrike, Parliaments

object ZebraPuzzle:
  export Resident.*

  case class Solution(waterDrinker: Resident, zebraOwner: Resident)

  lazy val solve: Solution =
    val residents = for
      xs        <- answers
      (r, d, p) <- xs
      if d == Drink.Water || p == Pet.Zebra
    yield (r, d)

    residents.toSeq match
      case r1 +: r2 +: Seq() if r1._2 == Drink.Water => Solution(r1._1, r2._1)
      case xs                                        => Solution(xs.tail.head._1, xs.head._1)

  private def answers: Iterator[Iterable[(Resident, Drink, Pet)]] =
    for
      colors <- Color.values.permutations
      if (Color.Ivory, colors) leftOf (Color.Green, colors) // 6
      residents <- Resident.values.permutations
      if ((Englishman, residents) same (Color.Red, colors)) && // 2
        first(Norwegian, residents) &&                         // 10
        ((Norwegian, residents) nextTo (Color.Blue, colors))   // 15
      drinks <- Drink.values.permutations
      if ((Drink.Coffee, drinks) same (Color.Green, colors)) && // 4
        ((Ukrainian, residents) same (Drink.Tea, drinks)) &&    // 5
        middle(Drink.Milk, drinks)                              // 9
      pets <- Pet.values.permutations
      if (Spaniard, residents) same (Pet.Dog, pets) // 3
      smokes <- Smoke.values.permutations
      if ((Smoke.OldGold, smokes) same (Pet.Snails, pets)) &&             // 7
        ((Smoke.Kools, smokes) same (Color.Yellow, colors)) &&            // 8
        ((Smoke.Chesterfields, smokes) nextTo (Pet.Fox, pets)) &&         // 1
        ((Smoke.Kools, smokes) nextTo (Pet.Horse, pets)) &&               // 12
        ((Smoke.LuckyStrike, smokes) same (Drink.OrangeJuice, drinks)) && // 13
        ((Smoke.Parliaments, smokes) same (Japanese, residents))          // 14
    yield residents.lazyZip(drinks).lazyZip(pets)

extension [A <: Enum](x: (A, Array[A]))
  infix private def same[B <: Enum](y: (B, Array[B])): Boolean =
    x._2.zip(y._2).contains((x._1, y._1))

  infix private def leftOf[B <: Enum](y: (B, Array[B])): Boolean =
    x same (y._1, y._2.tail)

  infix private def nextTo[B <: Enum](y: (B, Array[B])): Boolean =
    (x leftOf y) || (y leftOf x)

  private def middle: Boolean =
    x._2.indexOf(x._1) == 2

  private def first: Boolean =
    x._1 == x._2.head
