module Language.Embedded.VHDL.Monad.Util
  ( uType, uCast, uResize, uResizeBits
  -- utility.
  , maybePrimary, maybeLit, maybeVar, maybeFun, maybeExp
  , printPrimary
  , printBits
  --
  , expr
  , primRelation, primShift, primSimple, primTerm, primFactor
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

-- todo: handle bit case better.
-- todo: add naturals for unsigned integers.
uCast :: Expression -> SubtypeIndication -> SubtypeIndication -> Expression
uCast exp from to | isInteger from = go
  where
    -- Integer -> X
    go | isInteger  to = exp
       | isUnsigned to = expr $ toUnsigned exp $ expr $ width to
       | isSigned   to = expr $ toSigned   exp $ expr $ width to
       | isBits     to = expr $ asBits
                       $ expr $ toSigned exp
                       $ expr $ width to
       | otherwise     = exp
uCast exp from to | isUnsigned from = go
  where
    -- Unsigned -> X
    go | isInteger  to, Just lit <- maybeLit exp = exp
       | isInteger  to = expr $ toInteger exp
       | isUnsigned to = uResize exp from to
       | isSigned   to = expr $ asSigned $ uResize exp from to
       | isBits     to = expr $ asBits   $ uResize exp from to
       | otherwise     = exp
uCast exp from to | isSigned from = go
  where
    -- Signed -> X
    go | isInteger  to , Just lit <- maybeLit exp = exp
       | isInteger  to = expr $ toInteger exp
       | isUnsigned to = expr $ asUnsigned $ uResize exp from to
       | isSigned   to = uResize exp from to
       | isBits     to = expr $ asBits     $ uResize exp from to
       | otherwise     = exp
uCast exp from to | isBits from = go
  where
    -- Bits n -> X
    go | isInteger  to, Just lit <- maybeLit exp = exp
       | isInteger  to = expr $ toInteger $ expr $ asSigned exp
       | isUnsigned to = uResize (expr $ asUnsigned exp) from to
       | isSigned   to = uResize (expr $ asSigned exp) from to
       | isBits     to = uResizeBits exp from to
       | otherwise     = exp
uCast exp from to | isBit from, isBit to = exp
uCast exp from to =
  error $ "hardware-edsl.todo: missing type cast from ("
            ++ show (typeName from) ++ ") to ("
            ++ show (typeName to)   ++ ")."

uResize :: Expression -> SubtypeIndication -> SubtypeIndication -> Expression
uResize exp from to
  -- if literal, simply resize it.
  | Just p <- maybeLit exp = expr $ literal $ number $ printPrimary p to
  -- if variable, and types are equal, disregard resize.
  | Just v <- maybeVar exp, typeWidth from == typeWidth to = exp
  -- if already resized, disregard new resize.
  | Just w <- castWidth exp, w == typeWidth to = exp
  -- otherwise, resize.
  | otherwise = expr $ resize exp $ expr $ width to
  where
    

uResizeBits :: Expression -> SubtypeIndication -> SubtypeIndication -> Expression
uResizeBits exp from to
  -- if literal, simply resize it.
  | Just p <- maybeLit exp = expr $ literal $ number $ printPrimary p to
  -- if variable, and same size, disregard resize.
  | typeWidth from == typeWidth to = exp
  -- if target is smaller, slice source.
  | typeWidth from > typeWidth to
  , Just r <- typeRange to = expr $ name $ slice prefix r
  -- if target is larger, append zeroes.
  | typeWidth from < typeWidth to =
      let zeroes = name $ simple $ printBits (typeWidth to - typeWidth from) (0 :: Int)
          bits   = name $ prefix
          wrap s = ENand (Relation (ShiftExpression s Nothing) Nothing) Nothing
       in wrap (cat [primTerm (primFactor zeroes), primTerm (primFactor bits)])
  | otherwise = error $ show exp
  where
    prefix :: Name
    prefix | Just (PrimName n) <- maybeVar exp = n
           | Just (PrimFun  f) <- maybeFun exp = fc_function_name f
           | otherwise = error "hardware-edsl.slice: prefix of slice not var/fun."

--------------------------------------------------------------------------------

expr :: Primary -> Expression
expr (PrimExp e) = e
expr (primary)   = ENand (primRelation $ primShift $ primSimple $ primTerm $ primFactor primary) Nothing

primRelation :: ShiftExpression -> Relation
primRelation shift = Relation shift Nothing

primShift :: SimpleExpression -> ShiftExpression
primShift simple = ShiftExpression simple Nothing

primSimple :: Term -> SimpleExpression
primSimple term = SimpleExpression Nothing term []

primTerm :: Factor -> Term
primTerm factor = Term factor []

primFactor :: Primary -> Factor
primFactor primary = FacPrim primary Nothing

width :: SubtypeIndication -> Primary
width = literal . number . show . typeWidth

--------------------------------------------------------------------------------

maybePrimary :: Expression -> Maybe Primary
maybePrimary (ENand (Relation (ShiftExpression (SimpleExpression Nothing (Term (FacPrim p Nothing) []) []) Nothing) Nothing) Nothing) = Just p
maybePrimary _ = Nothing

maybeLit :: Expression -> Maybe Primary
maybeLit e | Just p@(PrimLit _) <- maybePrimary e = Just p
           | otherwise = Nothing

maybeVar :: Expression -> Maybe Primary
maybeVar e | Just p@(PrimName _) <- maybePrimary e = Just p
           | otherwise = Nothing

maybeFun :: Expression -> Maybe Primary
maybeFun e | Just p@(PrimFun _) <- maybePrimary e = Just p
           | otherwise = Nothing

maybeCast :: Expression -> Maybe Primary
maybeCast e | Just p@(PrimTCon _) <- maybePrimary e = Just p
            | otherwise = Nothing

maybeExp :: Expression -> Maybe Expression
maybeExp e | Just (PrimExp p) <- maybePrimary e = Just p
           | otherwise = Nothing

--------------------------------------------------------------------------------

castWidth :: Expression -> Maybe Integer
castWidth e
  | Just f       <- maybeFun e
  , Just (n, as) <- stripFun f = widthOf n as
  where
    widthOf :: String -> [Expression] -> Maybe Integer
    widthOf "resize"      [e, size] = stripNum =<< maybeLit size
    widthOf "to_signed"   [e, size] = stripNum =<< maybeLit size
    widthOf "to_unsigned" [e, size] = stripNum =<< maybeLit size
    widthOf "to_integer"  [e]       = Nothing -- todo: hmm?
    widthOf "signed"      [e]       = castWidth e
    widthOf "unsigned"    [e]       = castWidth e
    widthOf "std_logic_vector" [e]  = castWidth e
    widthOf _ _ = Nothing
castWidth _ = Nothing

--------------------------------------------------------------------------------

stripNum :: Primary -> Maybe Integer
stripNum (PrimLit (LitNum (NLitPhysical (PhysicalLiteral Nothing (NSimple (Ident i)))))) = Just (read i)
stripNum _ = Nothing

stripFun :: Primary -> Maybe (String, [Expression])
stripFun (PrimFun (FunctionCall (NSimple (Ident i)) Nothing)) = Just (i, [])
stripFun (PrimFun (FunctionCall (NSimple (Ident i)) (Just (AssociationList as)))) = Just (i, stripArgs as)
  where
    stripArgs :: [AssociationElement] -> [Expression]
    stripArgs [] = []
    stripArgs ((AssociationElement Nothing (APDesignator (ADExpression a))):as) = a : stripArgs as
stripFun _ = Nothing

stripPrimary :: Primary -> Maybe Expression
stripPrimary (PrimExp e) = Just e
stripPrimary _ = Nothing

--------------------------------------------------------------------------------

-- todo: this assumes i>0? and i<2^(width t)?
printPrimary :: Primary -> SubtypeIndication -> String
printPrimary p t = case (stripNum p) of
  Just i  -> printBits (typeWidth t) i
  Nothing -> error "hardware-edsl.printPrimary: not a literal."

printBits :: (PrintfArg a, PrintfType b) => Integer -> a -> b
printBits zeroes = printf ("\"%0" ++ show zeroes ++ "b\"")

--------------------------------------------------------------------------------
