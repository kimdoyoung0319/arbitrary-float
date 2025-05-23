/--
Precision of a floating point number.
-/
abbrev Prec := {n : Nat // n > 0}

namespace Prec

/--
Conversion between positive numbers and natural numbers.
-/
def toNat (n : Prec) : Nat := n

instance : OfNat Prec (n + 1) where
  ofNat := ⟨n + 1, Nat.succ_pos n⟩

/--
Propositional inequalities between precisions.
-/
def lt (n : Prec) (m : Prec) : Prop := LT.lt n.toNat m.toNat

/--
Propositional inequalities between precisions.
-/
def le (n : Prec) (m : Prec) : Prop := LE.le n.toNat m.toNat

instance : LT Prec where lt := Prec.lt

instance : LE Prec where le := Prec.le

instance {n : Prec} {m : Prec} : Decidable (n < m) :=
  inferInstanceAs (Decidable (n.toNat < m.toNat))

instance {n : Prec} {m : Prec} : Decidable (n ≤ m) :=
  inferInstanceAs (Decidable (n.toNat ≤ m.toNat))

/--
Maximum of two precisions.
-/
def max (n m : Prec) : Prec :=
  if n > m then n else m

instance : Max Prec where max := Prec.max

/--
Minimum of two precisions.
-/
def min (n m : Prec) : Prec :=
  if n > m then m else n

instance : Min Prec where min := Prec.min

end Prec
