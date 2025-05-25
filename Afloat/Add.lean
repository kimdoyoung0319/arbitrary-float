import Lean
import AFloat.Basic
import AFloat.Round
import AFloat.BitVec

namespace AFloat

/--
Adds two floating point numbers that has same sign. p is the desired precision of the result. m₁ and m₂ are mantissas of
operands. e₁ and e₂ are exponents. Returns the result of the addition.
-/
private def add' (p : Nat) : (BitVec p₁ × Int) → (BitVec p₂ × Int) → (BitVec p × Int) := fun (m₁, e₁) (m₂, e₂) =>
  let d := Int.natAbs (e₁ - e₂)
  let m₁ := if e₁ > e₂ then BitVec.setWidthLsb p m₁ else BitVec.setWidthLsb p (m₁.setWidthMsb (p₁ + d))
  let m₂ := if e₂ > e₁ then BitVec.setWidthLsb p m₂ else BitVec.setWidthLsb p (m₂.setWidthMsb (p₂ + d))
  let (c, m) := BitVec.addc m₁ m₂
  if c then (m.fillOneMsb.setWidthLsb p, max e₁ e₂ + 1) else (m.setWidthLsb p, max e₁ e₂)

/--
Subtracts the second number from the first number. p is the desired precision of the result. m₁ and m₂ are the mantissas
of operands. e₁ and e₂ are exponents. Assumes e₁ ≥ e₂. Returns the result of the subtraction.
-/
private def sub' (p : Nat) : (BitVec p₁ × Int) → (BitVec p₂ × Int) → (BitVec p × Int) := fun (m₁, e₁) (m₂, e₂) =>
  let (m₁, m₂) := BitVec.align m₁ m₂ (e₁ - e₂)
  let m := BitVec.sub m₁ m₂
  (m.setWidthLsb p, max e₁ e₂)

#eval ((0b11001#5) - (0b10100#5)).toString
#eval ((0b10100#5) - (0b11001#5)).toString

/--
Adds two floating point numbers, returns the result which is rounded to even with precision p.
-/
def add {p : Nat} : AFloat p₁ → AFloat p₂ → AFloat p
  | nan, _ | _, nan => nan
  | inf s₁, inf s₂ => if s₁ = s₂ then inf s₁ else nan
  | inf s, _ | _, inf s => inf s
  | zero s₁, zero s₂ => if s₁ = s₂ then zero s₁ else zero .pos
  | zero _, x | x, zero _ => Round.toNearest p x
  | fin s₁ m₁ e₁, fin s₂ m₂ e₂ =>
    let p' := (max p₁ (p₂ + Int.natAbs (e₁ - e₂))) + 1
    if s₁ = s₂ then
      let (m, e) := add' p' (m₁, e₁) (m₂, e₂)
      Round.toNearest p (fin s₁ m e)
    else
      let (m, e) := sub' p' (m₁, e₁) (m₂, e₂)
      dbg_trace m
      let x :=
        match compare e₁ e₂, m.msb with
        | .lt, _ => fin s₂ m e
        | .gt, _ => fin s₁ m e
        | .eq, true => fin s₁ (-m) e
        | .eq, false => fin s₂ m e
      Round.toNearest p (normalize x)

end AFloat
