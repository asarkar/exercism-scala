import java.lang.Math

extension (r: Robot)
  def turnRight: Robot = turn('R')
  def turnLeft: Robot  = turn('L')

  private def turn(c: Char): Robot =
    val bearings = Bearing.values.toIndexedSeq
    val i        = bearings.indexOf(r.bearing)
    val b = c match
      case 'L' => bearings(Math.floorMod(i - 1, 4))
      case _   => bearings((i + 1) % 4)

    Robot(b, r.coordinates)

  def advance: Robot =
    val (x, y) = r.coordinates
    val c = r.bearing match
      case Bearing.North => (x, y + 1)
      case Bearing.East  => (x + 1, y)
      case Bearing.South => (x, y - 1)
      case Bearing.West  => (x - 1, y)

    Robot(r.bearing, c)

  def simulate(ins: String): Robot = ins.foldLeft(r) { (acc, c) =>
    if c == 'A' then acc.advance else acc.turn(c)
  }
