enum Allergen:
  case Eggs, Peanuts, Shellfish, Strawberries, Tomatoes, Chocolate, Pollen, Cats

object Allergies:
  private val ordinalToAllergen = Allergen.values.zipWithIndex
    .map((a, i) => (i, a))
    .toMap
  private val ordinals = ordinalToAllergen.keys.toSeq.sorted

  def list(score: Int): Seq[Allergen] =
    ordinals
      // If the ith bit is set, the result is that 2^i (greater than 0)
      .filter(i => (score & (1 << i)) > 0)
      .map(ordinalToAllergen)

  def allergicTo(allergen: Allergen, score: Int): Boolean =
    list(score).contains(allergen)
