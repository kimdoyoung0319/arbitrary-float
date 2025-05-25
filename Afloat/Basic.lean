import AFloat.Sign
import AFloat.BitVec

/- TODO: Complete docstring. -/
/-!
* Precision: The number of bits used to store significant bits together with
             one bit over the decimal point.
-/

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
[b₀, b₁, b₂, ⋯, bₚ₋₁, bₚ] is the unique representation of a floating point
number of (-1)ˢ × bₚ₋₁.bₚ₋₂ ⋯ b₁b₀ × 2ᵉ.
-/
inductive AFloat (p : Nat) where
  | zero : AFloat.Sign → AFloat p
  | fin : AFloat.Sign → BitVec p → Int → AFloat p
  | inf : AFloat.Sign → AFloat p
  | nan : AFloat p
deriving BEq, DecidableEq

namespace AFloat

def neg : AFloat p → AFloat p
  | zero s => zero (-s)
  | fin s m e => fin (-s) m e
  | inf s => inf (-s)
  | nan => nan

instance : Neg (AFloat p) where
  neg := neg

/--
Auxiliary funcion that shifts the mantissa of a floating point number if its
MSB is 0.
-/
private partial def normalize' : AFloat p → AFloat p
  | fin s m e =>
    if m.msb == true then
      fin s m e
    else if m == 0 then
      zero .pos
    else
      normalize' (fin s (m <<< 1) (e - 1))
  | x => x

/--
Normalizes a floating point number so that the MSB of its mantissa to be 1.
-/
def normalize : AFloat p → AFloat p
  | fin s m e => normalize' (fin s m e)
  | x => x

private partial def toSuperScriptString' (n : Int) (acc : String) : String :=
  if n = 0 then
    acc
  else
    match n % 10 with
    | 0 => toSuperScriptString' (n / 10) ("⁰" ++ acc)
    | 1 => toSuperScriptString' (n / 10) ("¹" ++ acc)
    | 2 => toSuperScriptString' (n / 10) ("²" ++ acc)
    | 3 => toSuperScriptString' (n / 10) ("³" ++ acc)
    | 4 => toSuperScriptString' (n / 10) ("⁴" ++ acc)
    | 5 => toSuperScriptString' (n / 10) ("⁵" ++ acc)
    | 6 => toSuperScriptString' (n / 10) ("⁶" ++ acc)
    | 7 => toSuperScriptString' (n / 10) ("⁷" ++ acc)
    | 8 => toSuperScriptString' (n / 10) ("⁸" ++ acc)
    | 9 => toSuperScriptString' (n / 10) ("⁹" ++ acc)
    | _ => ""

private def toSuperScriptString (n : Int) : String :=
  if n ≥ 0 then
    toSuperScriptString' n ""
  else
    s!"⁻{(toSuperScriptString' (-n) "")}"

/-- Conversion to String. -/
def toString : AFloat p → String
  | zero s => s!"{s}0.0"
  | fin s m e => s!"{s}1.{BitVec.toString (m.setWidthMsb (p - 1))}×2{toSuperScriptString e}"
  | inf s => s.toString ++ "∞"
  | nan => "nan"

instance : ToString (AFloat p) where
  toString := toString

end AFloat
