module Language.Embedded.VHDL.Monad.Expression
  ( and, or, xor, xnor, nand, nor
  , eq, neq, lt, lte, gt, gte
  , sll, srl, sla, sra, rol, ror
  , add, sub, cat, neg
  , mul, div, mod, rem
  , exp, abs, not
  , name, string, indexed, selected, slice
  , lit, null
  , aggregate, associate
  , function
  , qualified
  , cast
  , resize
  , range, downto, to

  -- *** temp
  , attribute
  ) where

import Language.VHDL

import Prelude hiding (and, or, div, mod, rem, exp, abs, not, null)

--------------------------------------------------------------------------------
-- * Expressions and their sub-layers.
--------------------------------------------------------------------------------

relation :: RelationalOperator -> ShiftExpression -> ShiftExpression -> Relation
relation r a b = Relation a (Just (r, b))

shiftexp :: ShiftOperator -> SimpleExpression -> SimpleExpression -> ShiftExpression
shiftexp s a b = ShiftExpression a (Just (s, b))

simplexp :: Maybe Sign -> AddingOperator -> [Term] -> SimpleExpression
simplexp s o (a:as) = SimpleExpression s a (fmap ((,) o) as)

term :: MultiplyingOperator -> [Factor] -> Term
term o (a:as) = Term a (fmap ((,) o) as)

--------------------------------------------------------------------------------
-- ** Expressions

and, or, xor, xnor :: [Relation] -> Expression
and  = EAnd  
or   = EOr   
xor  = EXor  
xnor = EXnor

nand, nor :: Relation -> Relation -> Expression
nand a b = ENand a (Just b)
nor  a b = ENor  a (Just b)

--------------------------------------------------------------------------------
-- ** Relations

eq, neq, lt, lte, gt, gte :: ShiftExpression -> ShiftExpression -> Relation
eq  = relation Eq
neq = relation Neq
lt  = relation Lt
lte = relation Lte
gt  = relation Gt
gte = relation Gte

--------------------------------------------------------------------------------
-- ** Shift Expressions

sll, srl, sla, sra, rol, ror :: SimpleExpression -> SimpleExpression -> ShiftExpression
sll = shiftexp Sll
srl = shiftexp Srl
sla = shiftexp Sla
sra = shiftexp Sra
rol = shiftexp Rol
ror = shiftexp Ror

--------------------------------------------------------------------------------
-- ** Simple Expressions

add, sub, cat :: [Term] -> SimpleExpression
add = simplexp Nothing Plus
sub = simplexp Nothing Minus
cat = simplexp Nothing Concat

neg :: Term -> SimpleExpression
neg = simplexp (Just Negation) Plus . (: [])

--------------------------------------------------------------------------------
-- ** Terms

mul, div, mod, rem :: [Factor] -> Term
mul = term Times
div = term Div
mod = term Mod
rem = term Rem

--------------------------------------------------------------------------------
-- ** Factors

exp :: Primary -> Primary -> Factor
exp a b = FacPrim a (Just b)

abs, not :: Primary -> Factor
abs = FacAbs
not = FacNot

--------------------------------------------------------------------------------
-- ** Primaries

-- names
name :: Name -> Primary
name = PrimName

attribute :: String -> Primary -> Primary
attribute s (PrimName n) = PrimName $ NAttr $ AttributeName (PName n) Nothing (Ident s) Nothing

string :: String -> Primary
string = PrimLit . LitString . SLit

indexed :: Identifier -> Expression -> Name
indexed i l = NIndex $ IndexedName (PName $ NSimple i) [l]

selected  :: Identifier -> Identifier -> Name
selected p s = NSelect $ SelectedName (PName $ NSimple p) (SSimple s)

slice :: Identifier -> (SimpleExpression, SimpleExpression) -> Name
slice i (f, t) = NSlice $ SliceName (PName $ NSimple i) (DRRange $ RSimple f DownTo t)

-- literals
lit :: String -> Primary
lit = PrimLit . LitNum . NLitPhysical . PhysicalLiteral Nothing . NSimple . Ident

null :: Primary
null = PrimLit LitNull

-- aggregates
aggregate :: [Expression] -> Primary
aggregate = PrimAgg . Aggregate . fmap (ElementAssociation Nothing)

associate :: [(Maybe Choices, Expression)] -> Primary
associate es = PrimAgg $ Aggregate $ map (uncurry ElementAssociation) es

-- function calls
function :: Identifier -> [Expression] -> Primary
function i [] = PrimFun $ FunctionCall (NSimple i) Nothing
function i xs = PrimFun
  . FunctionCall (NSimple i) . Just . AssociationList
  $ fmap (AssociationElement Nothing . APDesignator . ADExpression) xs

-- qualified expressions
qualified :: SubtypeIndication -> Expression -> Primary
qualified (SubtypeIndication _ t _) = PrimQual . QualExp t

-- type conversions
cast :: SubtypeIndication -> Expression -> Primary
cast (SubtypeIndication _ t _) = PrimTCon . TypeConversion (unrange t)
  where
    unrange :: TypeMark -> TypeMark
    unrange (TMType (NSlice (SliceName (PName (NSimple (Ident typ))) _))) =
      TMType (NSimple (Ident typ))
    unrange (TMType (NSimple (Ident typ))) =
      TMType (NSimple (Ident typ))

--------------------------------------------------------------------------------
-- ** Utility

resize :: Expression -> Expression -> Primary
resize size exp = PrimFun $ FunctionCall name $ Just $ AssociationList [assoc size, assoc exp]
  where
    name  = NSimple $ Ident "resize"
    assoc = AssociationElement Nothing . APDesignator . ADExpression

range :: SimpleExpression -> Direction -> SimpleExpression -> Range
range = RSimple

downto, to :: Direction
downto = DownTo
to     = To

--------------------------------------------------------------------------------
