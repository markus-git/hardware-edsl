module Language.Embedded.VHDL.Monad.Type
  ( Type
  , Kind(..)

  , std_logic, std_logic_vector, std_logic_uvector
  , signed2,  signed4,  signed8,  signed16,  signed32,  signed64
  , usigned2, usigned4, usigned8, usigned16, usigned32, usigned64
  , float, double
  , integer

  , width
  , literal, point
  ) where

import Language.VHDL

import Language.Embedded.VHDL.Monad.Expression (lit)

--------------------------------------------------------------------------------
-- * VHDL Types.
--------------------------------------------------------------------------------

-- | Type indication for signals/variables/..
type Type = SubtypeIndication

-- | The different kinds of signals/variables/.. that exist in VHDL.
data Kind = Constant | Signal | Variable | File

--------------------------------------------------------------------------------
-- ** Standard logic types.

std_logic :: Type
std_logic = SubtypeIndication Nothing (TMType (NSimple (Ident "std_logic"))) Nothing

std_logic_vector :: Int -> Type
std_logic_vector range = SubtypeIndication Nothing
  (TMType (NSlice (SliceName
    (PName (NSimple (Ident "std_logic_vector")))
    (DRRange (RSimple (upper range) DownTo zero)))))
  (Nothing)

std_logic_uvector :: Type
std_logic_uvector = SubtypeIndication Nothing
  (TMType (NSimple (Ident "std_logic_vector")))
  (Nothing)

--------------------------------------------------------------------------------
-- ** Signed & unsigned numbers.

arith :: String -> Int -> Type
arith typ range = SubtypeIndication Nothing
  (TMType (NSlice (SliceName
    (PName (NSimple (Ident typ)))
    (DRRange (RSimple (upper range) DownTo zero)))))
  (Nothing)

signed, usigned :: Int -> Type
signed  = arith "signed"
usigned = arith "unsigned"

signed2, signed4, signed8, signed16, signed32, signed64 :: Type
signed2  = signed 2
signed4  = signed 4
signed8  = signed 8
signed16 = signed 16
signed32 = signed 32
signed64 = signed 64

usigned2, usigned4, usigned8, usigned16, usigned32, usigned64 :: Type
usigned2  = usigned 2
usigned4  = usigned 4
usigned8  = usigned 8
usigned16 = usigned 16
usigned32 = usigned 32
usigned64 = usigned 64

--------------------------------------------------------------------------------
-- ** Floating point.

floating :: Int -> Type
floating size = SubtypeIndication Nothing
  (TMType (NSimple (Ident ("float" ++ show size))))
  (Nothing)

float, double :: Type
float  = floating 32
double = floating 64

--------------------------------------------------------------------------------
-- ** ...

integer :: Maybe Range -> Type
integer r = SubtypeIndication Nothing
  (TMType (NSimple (Ident "integer")))
  (fmap (CRange . RangeConstraint) r)

--------------------------------------------------------------------------------
-- ** Helpers.

width :: Type -> Int
width (SubtypeIndication _ t r) = (unrange t + 1) * (maybe 1 range r)
  where
    unrange :: TypeMark -> Int
    unrange (TMType (NSlice (SliceName _ (DRRange (RSimple u DownTo l))))) =
      literal u - literal l
    unrange (TMType (NSlice (SliceName _ (DRRange (RSimple l To     u))))) =
      literal u - literal l

range :: Constraint -> Int
range (CRange (RangeConstraint (RSimple a DownTo b))) = literal a - literal b

--------------------------------------------------------------------------------

literal :: SimpleExpression -> Int
literal (SimpleExpression Nothing (Term (FacPrim i (Nothing)) []) []) = unlit i
  where
    unlit :: Primary -> Int
    unlit (PrimLit (LitNum (NLitPhysical (PhysicalLiteral Nothing (NSimple (Ident i)))))) = read i

point :: Show i => i -> SimpleExpression
point i = SimpleExpression Nothing (Term (FacPrim (lit $ show i) (Nothing)) []) []

upper 0 = point 0
upper n = point (n-1)
zero    = point 0

--------------------------------------------------------------------------------
