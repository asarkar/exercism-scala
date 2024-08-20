enum Bearing:
  case North, East, South, West

type Coordinates = (Int, Int)

case class Robot(bearing: Bearing, coordinates: Coordinates)
