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

/--
Splits a bitvector of length w into two bitvectors with length i and (w - i).

If i > w, the first bitvector is zero-padded while the second bitvector becomes
a bitvector with zero length.
-/
def split {w : Nat} (x : BitVec w) (i : Nat) : BitVec i × BitVec (w - i) :=
  (BitVec.setWidthMsb i x, BitVec.setWidthLsb (w - i) x)
