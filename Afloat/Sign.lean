/--
Signs possible for a floating point number.
-/
inductive Sign where
  | pos : Sign
  | neg : Sign
deriving Repr, BEq, DecidableEq

/--
Negation of a sign.
-/
def Sign.not : Sign → Sign
  | pos => neg
  | neg => pos

instance : Neg Sign where
  neg := Sign.not
