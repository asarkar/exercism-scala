object ProteinTranslation:
  private val translations = Seq(
    ("AUG", "Methionine"),
    ("UUU", "Phenylalanine"),
    ("UUC", "Phenylalanine"),
    ("UUA", "Leucine"),
    ("UUG", "Leucine"),
    ("UCU", "Serine"),
    ("UCC", "Serine"),
    ("UCA", "Serine"),
    ("UCG", "Serine"),
    ("UAU", "Tyrosine"),
    ("UAC", "Tyrosine"),
    ("UGU", "Cysteine"),
    ("UGC", "Cysteine"),
    ("UGC", "Cysteine"),
    ("UGG", "Tryptophan")
  ).toMap

  def proteins(s: String): Seq[String] =
    s
      .grouped(3)
      .map(x => translations.getOrElse(x, "STOP"))
      .takeWhile(_ != "STOP")
      .toSeq
