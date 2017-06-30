module Language.Embedded.VHDL.Monad.Type
  ( Type
  , Kind(..)
  , std_logic, std_logic_vector
  , signed2,  signed4,  signed8,  signed16,  signed32,  signed64
  , usigned2, usigned4, usigned8, usigned16, usigned32, usigned64
  , integer
  , float, double
  , unconstrainedArray, constrainedArray
  -- utility.
  , typeName, typeRange, typeWidth
  , isBit, isBits, isSigned, isUnsigned, isInteger
  ) where

import Language.VHDL

import Language.Embedded.VHDL.Monad.Expression
         ( literal
         , number
         , range
         , point
         , upper
         , zero )

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
std_logic = SubtypeIndication Nothing
  (TMType (NSimple (Ident "std_logic")))
  (Nothing)

std_logic_vector :: Integer -> Type
std_logic_vector range = SubtypeIndication Nothing
  (TMType (NSlice (SliceName
    (PName (NSimple (Ident "std_logic_vector")))
    (DRRange (RSimple (upper range) DownTo zero)))))
  (Nothing)

--------------------------------------------------------------------------------
-- ** Signed & unsigned numbers.

arith :: String -> Integer -> Type
arith typ range = SubtypeIndication Nothing
  (TMType (NSlice (SliceName
    (PName (NSimple (Ident typ)))
    (DRRange (RSimple (upper range) DownTo zero)))))
  (Nothing)

signed, usigned :: Integer -> Type
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
-- ** Integers.

integer :: Maybe Range -> Type
integer r = SubtypeIndication Nothing
  (TMType (NSimple (Ident "integer")))
  (fmap (CRange . RangeConstraint) r)

--------------------------------------------------------------------------------
-- ** Array Declarations.

compositeTypeDeclaration :: Identifier -> CompositeTypeDefinition -> TypeDeclaration
compositeTypeDeclaration name t = TDFull (FullTypeDeclaration name (TDComposite t))

unconstrainedArray :: Identifier -> SubtypeIndication -> TypeDeclaration
unconstrainedArray name typ = compositeTypeDeclaration name $
  CTDArray (ArrU (UnconstrainedArrayDefinition [] typ))

constrainedArray :: Identifier -> SubtypeIndication -> Range -> TypeDeclaration
constrainedArray name typ range = compositeTypeDeclaration name $
  CTDArray (ArrC (ConstrainedArrayDefinition
    (IndexConstraint [DRRange range]) typ))

--------------------------------------------------------------------------------
-- Utility.

typeName :: Type -> String
typeName (SubtypeIndication _ (TMType (NSimple (Ident n))) _) = n
typeName (SubtypeIndication _ (TMType (NSlice (SliceName (PName (NSimple (Ident n))) _))) _) = n

typeRange :: Type -> Maybe Range
typeRange (SubtypeIndication _ (TMType (NSlice (SliceName _ (DRRange r)))) _) = Just r
typeRange _ = Nothing

-- todo: this assumes we only use numbers when specifying ranges and constraints.
typeWidth :: Type -> Integer
typeWidth (SubtypeIndication _ t c) = (unrange t) * (maybe 1 unconstraint c)
  where
    unliteral :: SimpleExpression -> Integer
    unliteral (SimpleExpression _ (Term (FacPrim (PrimLit (LitNum (NLitPhysical (PhysicalLiteral _ (NSimple (Ident i)))))) _) _) _) =
      read i

    unrange :: TypeMark -> Integer
    unrange (TMType (NSlice (SliceName _ (DRRange (RSimple u DownTo l))))) =
      unliteral u - unliteral l + 1
    unrange (TMType (NSlice (SliceName _ (DRRange (RSimple l To     u))))) =
      unliteral u - unliteral l + 1

    unconstraint :: Constraint -> Integer
    unconstraint (CRange (RangeConstraint (RSimple u DownTo l))) =
      unliteral u - unliteral l + 1
    unconstraint (CRange (RangeConstraint (RSimple l To     u))) =
      unliteral u - unliteral l + 1

--------------------------------------------------------------------------------

isBit :: Type -> Bool
isBit t = "std_logic" == typeName t

isBits :: Type -> Bool
isBits t = "std_logic_vector" == typeName t

isSigned :: Type -> Bool
isSigned t = "signed" == typeName t

isUnsigned :: Type -> Bool
isUnsigned t = "unsigned" == typeName t

isInteger :: Type -> Bool
isInteger t = "integer" == typeName t

--------------------------------------------------------------------------------
