import AFloat.Sign

/--
An arbitrary precision floating point number is either zero, finite number,
infinity, or NaN. The type class parameter p stands for the precision of
the arbitrary precision floating point number format.

The zero, inf, and nan are similar to that of the IEEE 754 standard. The inf
constructor, takes a sign, vector of bits that represents the mantissa, and
an integer that corresponds to the exponent. Unlike the IEEE 754 standard or
the implementation in GNU MPFR, the exponent is interpreted as-is. In other
words, there's no such thing as a 'bias' that is added to the exponent.

Hence, a finite number constructed as AFloat.fin s m e where m =
[b₀, b₁, b₂, ⋯, bₚ₋₁] is the unique representation of a floating point number
of (-1)ˢ × bₚ₋₁.bₚ₋₂ ⋯ b₁b₀ × 2ᵉ.
-/
inductive AFloat (p : Nat) where
  | zero : Sign → AFloat p
  | fin : Sign → BitVec p → Int → AFloat p
  | inf : Sign → AFloat p
  | nan : AFloat p
deriving BEq

namespace AFloat

/--
Auxiliary funcion that shifts the mantissa of a floating point number if its
MSB is 0.
-/
private partial def normalize' : AFloat p → AFloat p
  | fin s m e =>
    if m.msb == true then
      fin s m e
    else if m == 0 then
      zero s
    else
      normalize' (fin s (m <<< 1) (e - 1))
  | x => x

/--
Normalizes a floating point number so that the MSB of its mantissa to be 1.
-/
def normalize : AFloat p → AFloat p
  | fin s m e => normalize' (fin s m e)
  | x => x

end AFloat
