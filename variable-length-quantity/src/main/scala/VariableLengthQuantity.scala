import java.lang.Integer

object VariableLengthQuantity:
  def encode(numbers: Seq[Int]): Seq[Int] =
    // Convert 32-bit signed int to 64-bit unsigned long
    // n & 0x00000000FFFFFFFFL
    numbers.flatMap(n => encode(Integer.toUnsignedLong(n)))

  /*
  1. Represent the value in binary notation (e.g. 137 as 10001001).
  2. Break it up in groups of 7 bits starting from the lowest
      significant bit (e.g. 137 as 0000001 0001001).
  3. Take the lowest 7 bits, and that gives the least significant
     byte (0000 1001). This byte comes last.
  4. For all the other groups of 7 bits (in the example, this is 000 0001),
     set the MSb to 1 (which gives 1000 0001 in our example).

     Thus. 137 becomes 1000 0001 0000 1001.
     By definition, the very last byte of a variable-length integer will
     have 0 as its MSb.
   */
  private def encode(n: Long): Seq[Int] =
    if n == 0 then Seq(0)
    else
      Seq
        .unfold(n) { i =>
          if i == 0 then None
          else
            // Mask (bitwise AND) with 0x7f = 127 = 0b1111111
            val x = i & 0x7f
            val y =
              // Clear the 8th bit from the right.
              // 0x80 = 128 = 0b10000000, ~128 = 0b01111111
              if i == n then x & ~0x80
              // Set the 8th bit from the right.
              else x | 0x80
            // Right shift the remaining num dropping
            // the 7 bits processed above.
            Some((y.toInt, i >> 7))
        }
        .reverse

  private def isLastByte(n: Long): Boolean =
    (n & 0x80) == 0

  def decode(numbers: Seq[Int]): Either[String, Seq[Int]] =
    if !numbers.isEmpty && !isLastByte(numbers.last) then Left("incomplete sequence")
    else
      /*
      The following loop explained:
      1. Take a byte and mask it ('&' = bitwise AND) with
         0x7f (0b1111111), in other words - extract the last
         7 bits of the current byte.
      2. Left shift ('<<' = bitwise left shift) the current num
         by 7, meaning it creates 7 zeroes on the right hand side
         of the binary number to make space for the 7 extracted
         bits from step 1.
      3. Using bitwise OR ('|') insert the extracted 7 bits form step 1
         into the previously shifted num from step 2.
      4. Loop until a byte with MSb 0 is found.

      Steps 1 through 3 are all happening within the line:
      ==>   (i << 7) | (n & 0x7f)   <==
       */
      Right(
        numbers
          .foldLeft(0, Seq.empty[Int]) { case ((i, acc), n) =>
            val x = (i << 7) | (n & 0x7f)
            if isLastByte(n) then (0, x +: acc)
            else (x, acc)
          }
          ._2
          .reverse
      )
