case class ComplexNumber(real: Double = 0.0d, imaginary: Double = 0.0d):
  def *(other: ComplexNumber): ComplexNumber =
    val r = real * other.real - imaginary * other.imaginary
    val i = imaginary * other.real + real * other.imaginary

    ComplexNumber(r, i)

  def +(other: ComplexNumber): ComplexNumber =
    val r = real + other.real
    val i = imaginary + other.imaginary

    ComplexNumber(r, i)

  def -(other: ComplexNumber): ComplexNumber =
    val r = real - other.real
    val i = imaginary - other.imaginary

    ComplexNumber(r, i)

  def /(other: ComplexNumber): ComplexNumber =
    val x = other.real * other.real + other.imaginary * other.imaginary
    val r = (real * other.real + imaginary * other.imaginary) / x
    val i = (imaginary * other.real - real * other.imaginary) / x

    ComplexNumber(r, i)

  def abs: Double =
    math.sqrt(real * real + imaginary * imaginary)

  def conjugate: ComplexNumber =
    ComplexNumber(real, -imaginary)

object ComplexNumber:
  /*
    e^(a + i * b)
        = e^a * e^(i * b)
        = e^a * (cos(b) + i * sin(b))
        = e^a * cos(b) + i * sin(b) * e^a
   */
  def exp(z: ComplexNumber): ComplexNumber =
    val x = math.pow(math.E, z.real)
    val r = x * math.cos(z.imaginary)
    val i = x * math.sin(z.imaginary)

    ComplexNumber(r, i)
