object SpaceAge:
  private val EARTH_YEAR_SECONDS = 31557600.0d

  private def age(age: Double, orbitalPeriod: Double) =
    val x = age / (EARTH_YEAR_SECONDS * orbitalPeriod)
    // https://stackoverflow.com/q/11106886/839733
    (x * 100).round / 100.toDouble

  def onEarth(age: Double): Double = this.age(age, 1.0)

  def onVenus(age: Double): Double = this.age(age, 0.61519726)

  def onMercury(age: Double): Double = this.age(age, 0.2408467)

  def onMars(age: Double): Double = this.age(age, 1.8808158)

  def onJupiter(age: Double): Double = this.age(age, 11.862615)

  def onSaturn(age: Double): Double = this.age(age, 29.447498)

  def onUranus(age: Double): Double = this.age(age, 84.016846)

  def onNeptune(age: Double): Double = this.age(age, 164.79132)
