object Bob:
  def response(statement: String): String =
    val s        = statement.filter(c => c.isLetterOrDigit || c == '?')
    val numUpper = s.count(_.isUpper)
    val numLower = s.count(_.isLower)

    val question = s.endsWith("?")
    val address  = s.isEmpty
    val yell     = numUpper > 0 && numLower == 0

    if address then "Fine. Be that way!"
    else if yell && question then "Calm down, I know what I'm doing!"
    else if yell then "Whoa, chill out!"
    else if question then "Sure."
    else "Whatever."
