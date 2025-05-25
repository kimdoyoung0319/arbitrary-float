import AFloat.Basic
import AFloat.Add
import AFloat.Comp

namespace AFloat.Test

private def x := fin .pos 0b0010#4 0 -- +0.010×2⁰

/- Normalization of a number. -/
#guard (normalize x) == (fin .pos 0b1000#4 (-2))

private def x₁ := fin .pos (0b11101#5) 0 -- +1.1101×2⁰
private def x₂ := fin .pos (0b10100#5) 0 -- +1.0100×2⁰
private def x₃ := fin .pos (0b11101#5) 3 -- +1.1101×2³
private def x₄ := fin .neg (0b11101#5) 0 -- -1.1101×2⁰
private def x₅ := fin .neg (0b10100#5) 0 -- -1.0100×2⁰
private def x₆ := fin .neg (0b11101#5) 3 -- -1.1101×2³

/- Rounding of a number. -/
#guard (Round.toNearest 3 x₁) = (fin .pos (0b111#3) 0)
#guard (Round.toNearest 2 x₂) = (fin .pos (0b10#2) 0)
#guard (Round.toNearest 2 x₃) = (fin .pos (0b10#2) 4)

/- Addition between two positive numbers whose mantissas cause a carry. -/
#guard (add x₁ x₂) = (fin .pos 0b110001#6 1)

/- Addition between two positive numbers whose mantissas does not cause a carry. -/
#guard (add x₂ x₂) = (fin .pos 0b101000#6 1)

/- Addition between two positive numbers that has different exponent. -/
#guard (add x₁ x₃) == (fin .pos 0b100000101#9 4)

/- Addition between two numbers with different signs. -/
#guard (add x₁ x₄) == (zero .pos : AFloat 5)    -- +0.0
#guard (add x₂ x₄) == (fin .neg 0b10010#5 (-1)) -- -1.0010×2⁻¹
#guard (add x₃ x₄) == (fin .pos 0b110011#6 3)   -- +1.10011×2³
#guard (add x₁ x₅) == (fin .pos 0b10010#5 (-1)) -- +1.0010×2⁻¹
#guard (add x₁ x₆) == (fin .neg 0b11001011#8 3) -- -1.1001011×2³

/- TODO: Add a test for the case where exponent decreases after addition. -/

end AFloat.Test
