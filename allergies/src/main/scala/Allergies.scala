enum Allergen:
  case Eggs, Peanuts, Shellfish, Strawberries, Tomatoes, Chocolate, Pollen, Cats

object Allergies:
  def list(score: Int): Seq[Allergen] =
    val allergens = Allergen.values
    allergens.indices
      // If the ith bit is set, the result is that 2^i (greater than 0)
      .filter(i => (score & (1 << i)) > 0)
      .map(allergens)

  def allergicTo(allergen: Allergen, score: Int): Boolean =
    list(score).contains(allergen)
