class Robot:
    private val _name = Robot.mkName()

    def reset(): Unit =
        Robot.names(_name) = Robot.mkName()

    def name: String = Robot.names(_name)

object Robot:
    import scala.util.Random

    private val names = collection.mutable.Map.empty[String, String]
    private val alpha = ('A' to 'Z').mkString
    private val nums = (0 to 9).mkString

    private def select(xs: String): Char = xs(Random.nextInt(xs.size))

    private def mkName(): String =
        // Much faster than Random.shuffle(xs).take(n).mkString
        val sb = new StringBuilder()
        sb += select(alpha)
        sb += select(alpha)
        sb += select(nums)
        sb += select(nums)
        sb += select(nums)
        val name = sb.toString()

        if names.contains(name) then
            mkName()
        else
            names(name) = name
            name
