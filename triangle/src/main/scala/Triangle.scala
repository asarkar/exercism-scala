class Triangle(x: Float, y: Float, z: Float):
  private val nonZeroSides          = x > 0 && y > 0 && z > 0
  private val validLengthInequality = (x + y > z) && (x + z > y) && (y + z > x)

  val equilateral: Boolean = nonZeroSides && validLengthInequality && x == y && y == z
  val isosceles: Boolean   = nonZeroSides && validLengthInequality && (x == y || y == z || x == z)
  val scalene: Boolean     = nonZeroSides && validLengthInequality && !isosceles

object Triangle:
  def apply(x: Float, y: Float, z: Float): Triangle =
    new Triangle(x, y, z)
