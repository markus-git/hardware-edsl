module Language.Embedded.VHDL.Monad.Type where

import Language.VHDL
import Language.Embedded.VHDL.Expression.Hoist hiding (Kind) -- resize
import Language.Embedded.VHDL.Monad.Expression (lit)

--------------------------------------------------------------------------------
-- * Types
--------------------------------------------------------------------------------

-- | Type indication for signals/variables/..
type Type = SubtypeIndication

-- | The different kinds of signals/variables/.. that exist in VHDL
data Kind = Constant | Signal | Variable | File

--------------------------------------------------------------------------------
-- ** Standard logic types

std_logic :: Type
std_logic = SubtypeIndication Nothing (TMType (NSimple (Ident "std_logic"))) Nothing

--------------------------------------------------------------------------------
-- ** Signed / Unsigned

arith :: String -> Int -> Type
arith typ range = SubtypeIndication Nothing
    (TMType (NSlice (SliceName
      (PName (NSimple (Ident typ)))
      (DRRange (RSimple (point (range - 1)) DownTo (point 0))))))
    (Nothing)
  where
    point :: Int -> SimpleExpression
    point i = SimpleExpression Nothing (Term (FacPrim (lit (show i)) (Nothing)) []) []

signed, usigned :: Int -> Type
signed  = arith "signed"
usigned = arith "unsigned"
  
signed8, signed16, signed32, signed64 :: Type
signed8  = signed 8
signed16 = signed 16
signed32 = signed 32
signed64 = signed 64

usigned8, usigned16, usigned32, usigned64 :: Type
usigned8  = usigned 8
usigned16 = usigned 16
usigned32 = usigned 32
usigned64 = usigned 64

-- .. add more ..

--------------------------------------------------------------------------------
-- ** Type conversion (and other stuff)

resize :: Int -> Term -> Primary
resize size exp =
  PrimFun (FunctionCall
    (NSimple (Ident "resize"))
    (Just (AssociationList
      [ AssociationElement Nothing (APDesignator (ADExpression (lift exp)))
      , AssociationElement Nothing (APDesignator (ADExpression (lift (lit (show size)))))
      ])
    ))


--------------------------------------------------------------------------------
