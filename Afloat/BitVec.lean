namespace BitVec

/--
Transforms a bitvector of length w into a bitvector of length v, padding with 0
as needed. Does the opposite of BitVec.truncate. This modifies low bits instead
of high bits.

The specific behavior depends on the relationship between the starting width w
and the final width v:

* If v > w, it is zero-extended; the low bits are padded with zeroes until the
bitvector has v bits.
* If v = w, the bitvector is returned unchanged.
* If v < w, the low bits are truncated.
-/
def setWidthLsb {w : Nat} (v : Nat) (x : BitVec w) : BitVec v :=
  if v ≤ w then
    .ofNat v (x.toNat >>> (w - v))
  else
    .ofNat v (x.toNat <<< (v - w))

/--
Transforms a bitvector of length w into a bitvector of length v, padding with 0
as needed.

The specific behavior depends on the relationship between the starting width w
and the final width v:

* If v > w, it is zero-extended; the high bits are padded with ones until the
bitvector has v bits.
* If v = w, the bitvector is returned unchanged.
* If v < w, the high bits are truncated.
-/
def setWidthMsb {w : Nat} (v : Nat) (x : BitVec w) : BitVec v :=
  x.zeroExtend v

/--
Transforms a bitvector of length w into a bitvector of length v, padding with 1
as needed.

The specific behavior depends on the relationship between the starting width w
and the final width v:

* If v > w, it is zero-extended; the high bits are padded with ones until the
bitvector has v bits.
* If v = w, the bitvector is returned unchanged.
* If v < w, the high bits are truncated.
-/
def fillOneMsb {w : Nat} (x : BitVec w) : BitVec (1 + w) :=
  (0b1#1) ++ x

/- TODO: Delete this. -/
/--
Splits a bitvector of length w into two bitvectors with length i and (w - i).

If i > w, the first bitvector is zero-padded while the second bitvector becomes
a bitvector with zero length.
-/
def split {w : Nat} (x : BitVec w) (i : Nat) : BitVec i × BitVec (w - i) :=
  (BitVec.setWidthMsb i x, BitVec.setWidthLsb (w - i) x)

/- TODO: Add comment. -/
def addc {w : Nat} (x y : BitVec w) : Bool × BitVec w := BitVec.adc x y false

/- TODO: Add comment. -/
private partial def toString' {w : Nat} (x : BitVec w) (acc : String) : String :=
  if acc.length = w then
    acc
  else if x.msb then
    toString' (x <<< 1) (acc ++ "1")
  else
    toString' (x <<< 1) (acc ++ "0")

def toString {w : Nat} (x : BitVec w) : String := toString' x ""

instance : ToString (BitVec w) where
  toString := toString

/- TODO: Add comment. -/
def align (m₁ : BitVec p₁) (m₂ : BitVec p₂) (expDiff : Int) :=
  let resultPrec := if expDiff > 0 then max p₁ (p₂ + expDiff.natAbs) else max (p₁ + expDiff.natAbs) p₂
  if expDiff > 0 then
    (m₁.setWidthLsb resultPrec, setWidthLsb resultPrec (m₂.setWidthMsb (p₂ + expDiff.natAbs)))
  else
    (m₂.setWidthLsb resultPrec, setWidthLsb resultPrec (m₁.setWidthMsb (p₁ + expDiff.natAbs)))

end BitVec
