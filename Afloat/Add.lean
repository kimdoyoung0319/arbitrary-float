import AFloat.Basic
import AFloat.Round
import AFloat.BitVec

namespace AFloat

/--
Adds two floating point numbers that has same sign. p is the desired
precision of the result. m₁ and m₂ are mantissas of operands. e₁ and e₂ are
exponents. Assumes e₁ > e₂. Returns the result of the addition, which may be a
subnormal number.
-/
private def add'
  {p : Nat} : (BitVec p₁ × Int) → (BitVec p₂ × Int) → (BitVec p × Int)
  | (m₁, e₁), (m₂, e₂) =>
    let d := Int.toNat (e₁ - e₂)
    let p' := Nat.max p₁ (p₂ + d)
    let m₁ := BitVec.setWidthLsb p' m₁
    let m₂ := BitVec.setWidthLsb p' (m₂.setWidthMsb (p₂ + d))
    let (c, m) := BitVec.adc m₁ m₂ false
    if c then
      (m.fillOneMsb.setWidthLsb p, e₁ + 1)
    else
      (m.setWidthLsb p, e₁)

/--
Adds two floating point numbers, returns the result which is rounded to even
with precision p.
-/
def add {p : Nat} : AFloat p₁ → AFloat p₂ → AFloat p
  | nan, _ | _, nan => nan
  | inf s₁, inf s₂ => if s₁ = s₂ then inf s₁ else nan
  | inf s, _ | _, inf s => inf s
  | zero s₁, zero s₂ => if s₁ = s₂ then zero s₁ else zero .pos
  | zero _, x | x, zero _ => Round.toNearest p x
  | fin s₁ m₁ e₁, fin s₂ m₂ e₂ =>
    if s₁ = s₂ then
      if e₁ > e₂ then
        let (m, e) := add' (m₁, e₁) (m₂, e₂)
        fin s₁ m e
      else
        let (m, e) := add' (m₂, e₂) (m₁, e₁)
        fin s₁ m e
    else
      nan

end AFloat
