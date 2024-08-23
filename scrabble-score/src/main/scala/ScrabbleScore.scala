object ScrabbleScore:
  def score(s: String): Int = s.toUpperCase().map(score).sum

  private def score(c: Char): Int =
    if "AEIOULNRST".contains(c) then 1
    else if "DG".contains(c) then 2
    else if "BCMP".contains(c) then 3
    else if "FHVWY".contains(c) then 4
    else if c == 'K' then 5
    else if "JX".contains(c) then 8
    else if "QZ".contains(c) then 10
    else 0
