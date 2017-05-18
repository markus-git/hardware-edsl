module Language.Embedded.VHDL.Monad.Util
  ( maybeLit, maybeVar
  , uType, uCast, uResize
  , printPrimary
  ) where

import Language.VHDL
import Language.Embedded.VHDL.Monad.Expression
import Language.Embedded.VHDL.Monad.Type

import Text.Printf

import Prelude hiding (toInteger)

--------------------------------------------------------------------------------
-- * Temp (still working on these).
--------------------------------------------------------------------------------

maybeLit :: Expression -> Maybe Primary
maybeLit (ENand (Relation (ShiftExpression (SimpleExpression Nothing (Term (FacPrim p@(PrimLit _) Nothing) []) []) Nothing) Nothing) Nothing) = Just p
maybeLit _ = Nothing

maybeVar :: Expression -> Maybe Primary
maybeVar (ENand (Relation (ShiftExpression (SimpleExpression Nothing (Term (FacPrim p@(PrimName _) Nothing) []) []) Nothing) Nothing) Nothing) = Just p
maybeVar _ = Nothing

--------------------------------------------------------------------------------

uType :: Expression -> SubtypeIndication -> Expression
uType exp to = uCast exp to to

uCast :: Expression -> SubtypeIndication -> SubtypeIndication -> Expression
uCast exp from to = case (isInteger from) of
  -- I'm an integer.
  Just True -> case (isSigned to) of
    -- Integer -> IntX
    Just True  -> primary $ toSigned exp $ primary $ uWidth to
    -- Integer -> WordX
    Just False -> primary $ toUnsigned exp $ primary $ uWidth to
    -- Integer -> Integer
    Nothing    -> exp
  Nothing   -> case (isSigned from) of
    -- I'm signed.
    Just True  -> case (isSigned to) of
      -- IntX -> IntX
      Just True  -> uResize exp from to
      -- IntX -> WordX
      Just False -> primary $ asUnsigned $ uResize exp from to
      -- IntX -> Integer
      Nothing    -> primary $ uInteger exp
    -- I'm unsigned.
    Just False -> case (isSigned to) of
      -- WordX -> WordX
      Just False -> uResize exp from to
      -- WordX -> IntX
      Just True  -> primary $ asSigned $ uResize exp from to
      -- WordX -> Integer
      Nothing    -> primary $ uInteger exp
    -- I'm what now?
    Nothing -> error "hardware-edsl: missing sym for type casting."

uWidth :: SubtypeIndication -> Primary
uWidth = lit . show . width

uResize :: Expression -> SubtypeIndication -> SubtypeIndication -> Expression
uResize exp from to
  | Just p <- maybeLit exp = primary $ lit $ printPrimary p to
  | Just v <- maybeVar exp
  , width from == width to = exp
  | otherwise = primary $ resize exp $ primary $ uWidth to

uInteger :: Expression -> Primary
uInteger exp
  | Just p <- maybeLit exp = p
  | otherwise = toInteger exp

--------------------------------------------------------------------------------

-- todo: this assumes i>0? and i<2^(width t)?
printPrimary :: Primary -> SubtypeIndication -> String
printPrimary p t = case (unlit p) of
  Just i  -> printBits (width t) i
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
