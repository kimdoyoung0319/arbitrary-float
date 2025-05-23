import AFloat.Basic
import AFloat.Add

namespace AFloat.Test

private def x := fin .pos 0b0010#4 0 -- +0.010×2⁰

/- Normalization of a number. -/
#guard (normalize x) == (fin .pos 0b1000#4 (-2))

private def x₁ := fin .pos (0b1101#4) 0 -- +1.101×2⁰
private def x₂ := fin .pos (0b0100#4) 0 -- +0.010×2⁰
private def x₃ := fin .pos (0b1101#4) 3 -- +1.101×2³

/- Addition between two numbers whose mantissas cause a carry. -/
#guard (add x₁ x₂) == (fin .pos 0b10#2 1)

/- Addition between two numbers whose mantissas does not cause a carry. -/
#guard (add x₂ x₂) == (fin .pos 0b10#2 1)

/- Addition between two numbers that has different exponent. -/
#guard (add x₁ x₃) == (fin .pos 0b000#4 4)

end AFloat.Test
