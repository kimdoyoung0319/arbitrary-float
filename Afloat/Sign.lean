namespace AFloat

/--
Signs possible for a floating point number.
-/
inductive Sign where
  | pos : Sign
  | neg : Sign
deriving Repr, BEq, DecidableEq

namespace Sign

/--
Negation of a sign.
-/
def not : Sign → Sign
  | pos => neg
  | neg => pos

instance : Neg Sign where
  neg := not

/--
Conversion to String.
-/
def toString : Sign → String
  | pos => "+"
  | neg => "-"

instance : ToString Sign where
  toString := toString

end Sign

end AFloat
