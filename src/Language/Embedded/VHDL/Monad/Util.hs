module Language.Embedded.VHDL.Monad.Util
  ( maybeLit, maybeVar
  , uType, uCast, uResize
  , printPrimary, printBits
  ) where

import Language.VHDL
import Language.Embedded.VHDL.Monad.Expression
import Language.Embedded.VHDL.Monad.Type

import Text.Printf

import Prelude hiding (toInteger)

--------------------------------------------------------------------------------
-- * Temp (still working on these).
--------------------------------------------------------------------------------

uType :: Expression -> SubtypeIndication -> Expression
uType exp to = uCast exp to to

-- todo: handle bit.
-- todo: add naturals for unsigned integers.
uCast :: Expression -> SubtypeIndication -> SubtypeIndication -> Expression
uCast exp from to | isInteger from = go
  where
    go | isInteger  to = exp
       | isUnsigned to = primary $ toUnsigned exp $ primary $ uWidth to
       | isSigned   to = primary $ toSigned   exp $ primary $ uWidth to
       | isBits     to = primary $ asBits
                       $ primary $ toSigned exp
                       $ primary $ uWidth to
       | otherwise     = exp
uCast exp from to | isUnsigned from = go
  where
    go | isInteger  to, Just lit <- maybeLit exp = exp
       | isInteger  to = primary $ toInteger exp
       | isUnsigned to = uResize exp from to
       | isSigned   to = primary $ asSigned $ uResize exp from to
       | isBits     to = primary $ asBits   $ uResize exp from to
       | otherwise     = exp
uCast exp from to | isSigned from = go
  where
    go | isInteger  to , Just lit <- maybeLit exp = exp
       | isInteger  to = primary $ toInteger exp
       | isUnsigned to = primary $ asUnsigned $ uResize exp from to
       | isSigned   to = uResize exp from to
       | isBits     to = primary $ asBits     $ uResize exp from to
       | otherwise     = exp
uCast exp from to | isBits from = go
  where
    go | isInteger  to, Just lit <- maybeLit exp = exp
       | isInteger  to = primary $ toInteger  $ primary $ asSigned exp
       | isUnsigned to = primary $ asUnsigned $ uResize exp from to
       | isSigned   to = primary $ asSigned   $ uResize exp from to
       | isBits     to = uResize exp from to
       | otherwise     = exp
uCast exp from to | isBit from, isBit to = exp
uCast exp from to =
  error $ "hardware-edsl.todo: missing cast from ("
            ++ show from ++ ") to ("
            ++ show to   ++ ")."

uWidth :: SubtypeIndication -> Primary
uWidth = lit . show . typeWidth

uResize :: Expression -> SubtypeIndication -> SubtypeIndication -> Expression
uResize exp from to
  | Just p <- maybeLit exp = primary $ lit $ printPrimary p to
  | Just v <- maybeVar exp, typeWidth from == typeWidth to = exp
  | otherwise = primary $ resize exp $ primary $ uWidth to

uLiteral :: Expression -> Primary
uLiteral exp
  | Just p <- maybeLit exp = p
  | otherwise = toInteger exp

--------------------------------------------------------------------------------

maybeLit :: Expression -> Maybe Primary
maybeLit (ENand (Relation (ShiftExpression (SimpleExpression Nothing (Term (FacPrim p@(PrimLit _) Nothing) []) []) Nothing) Nothing) Nothing) = Just p
maybeLit _ = Nothing

maybeVar :: Expression -> Maybe Primary
maybeVar (ENand (Relation (ShiftExpression (SimpleExpression Nothing (Term (FacPrim p@(PrimName _) Nothing) []) []) Nothing) Nothing) Nothing) = Just p
maybeVar _ = Nothing

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

-- todo: this assumes i>0? and i<2^(width t)?
printPrimary :: Primary -> SubtypeIndication -> String
printPrimary p t = case (unlit p) of
  Just i  -> printBits (typeWidth t) i
  Nothing -> error "hardware-edsl.printPrimary: not a literal."

-- todo: copy of the one in 'Represent.hs', should move one of them.
printBits :: (PrintfArg a, PrintfType b) => Int -> a -> b
printBits zeroes = printf ("\"%0" ++ show zeroes ++ "b\"")

--------------------------------------------------------------------------------

primary :: Primary -> Expression
primary p = ENand (Relation (ShiftExpression (SimpleExpression Nothing (Term (FacPrim p Nothing) []) []) Nothing) Nothing) Nothing

unlit :: Primary -> Maybe Integer
unlit (PrimLit (LitNum (NLitPhysical (PhysicalLiteral Nothing (NSimple (Ident i)))))) = Just (read i)
unlit _ = Nothing

--------------------------------------------------------------------------------
{-
-- todo: this is a more general kind of traversal, I could expose that later on.
asDec :: Expression -> Expression
asDec (ENand (Relation (ShiftExpression (SimpleExpression Nothing (Term (FacPrim (PrimExp e) Nothing) []) []) Nothing) Nothing) Nothing) = asDec e
asDec e = expDec e
  where
    expDec :: Expression -> Expression
    expDec e = case e of
      (EAnd  rs)   -> EAnd  (map relDec rs)
      (EOr   rs)   -> EOr   (map relDec rs)
      (EXor  rs)   -> EXor  (map relDec rs)
      (EXnor rs)   -> EXnor (map relDec rs)
      (ENand r mr) -> ENand (relDec r) (fmap relDec mr)
      (ENor  r mr) -> ENor  (relDec r) (fmap relDec mr)
    
    relDec :: Relation -> Relation
    relDec (Relation sh m) = Relation (shiftDec sh) (fmap (fmap shiftDec) m)

    shiftDec :: ShiftExpression -> ShiftExpression
    shiftDec (ShiftExpression si m) = ShiftExpression (simpleDec si) (fmap (fmap simpleDec) m)

    simpleDec :: SimpleExpression -> SimpleExpression
    simpleDec (SimpleExpression m t ts) = SimpleExpression m (termDec t) (fmap (fmap termDec) ts)

    termDec :: Term -> Term
    termDec (Term f fs) = Term (facDec f) (fmap (fmap facDec) fs)

    facDec :: Factor -> Factor
    facDec f = case f of
      (FacPrim p mp) -> FacPrim (primDec p) (fmap primDec mp)
      (FacAbs  p)    -> FacAbs p
      (FacNot  p)    -> FacNot p
    
    primDec :: Primary -> Primary
    primDec p = case p of
      (PrimExp e) -> PrimExp (expDec e)
      (PrimLit l) -> litAsDec (PrimLit l)
      (other)     -> other

litAsDec :: Primary -> Primary
litAsDec (PrimLit (LitNum (NLitPhysical (PhysicalLiteral Nothing (NSimple (Ident i)))))) = (PrimLit (LitNum (NLitPhysical (PhysicalLiteral Nothing (NSimple (Ident j))))))
  where
    j :: String
    j = show $ showDec (trim i)
    
    trim :: String -> String
    trim ('\"':xs) = init xs
    trim xs = xs

    showDec :: String -> Int
    showDec = foldl' (\acc x -> acc * 2 + digitToInt x) 0
-}
--------------------------------------------------------------------------------
