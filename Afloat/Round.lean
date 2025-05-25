import AFloat.Basic
import AFloat.BitVec

/--
Type for rounding modes for AFloat. A rounding mode is a function that
converts an AFloat with precision m to an AFloat with precision n.
-/
abbrev Round (p₁ : Nat) (p₂ : Nat) := AFloat p₁ → AFloat p₂

namespace Round

/--
Rounds an floating point number with precision p₁ toward zero with precision p₂.
-/
def towardZero {p₁ : Nat} (p₂ : Nat) : Round p₁ p₂
  | .zero s => .zero s
  | .inf s => .inf s
  | .fin s m e => .fin s (m.setWidthLsb p₂) e
  | .nan => .nan

/--
Rounds an floating point number precision p₁, away from zero with precision p₂.
-/
def awayFromZero {p₁ : Nat} (p₂ : Nat) : Round p₁ p₂
  | .zero s => .zero s
  | .inf s => .inf s
  | .nan => .nan
  | .fin s m e =>
    let (c, m') := BitVec.adc (m.setWidthLsb p₂) 1 false
    if c then .fin s (m'.fillOneMsb.setWidthLsb p₂) (e + 1) else .fin s m' e

/--
Rounds an floating point number with precision p₁, to nearest floating point
number with precision p₂, tying to an even number.
-/
def toNearest {p₁ : Nat} (p₂ : Nat) : Round p₁ p₂
  | .zero s => .zero s
  | .inf s => .inf s
  | .nan => .nan
  | .fin s m e =>
    let x := .fin s m e
    if p₁ ≤ p₂ then
      Round.towardZero p₂ x
    else
      let r := m.getMsbD p₂
      let f := (m.setWidthMsb (p₁ - p₂ - 1)).toFin != 0
      match r, f with
      | false, _ => Round.towardZero p₂ x
      | true, true => Round.awayFromZero p₂ x
      | true, false =>
        let g := m.getMsbD (p₂ - 1)
        if g then Round.awayFromZero p₂ x else Round.towardZero p₂ x

end Round
