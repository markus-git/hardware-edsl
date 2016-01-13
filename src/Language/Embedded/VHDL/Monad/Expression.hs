module Language.Embedded.VHDL.Monad.Expression where

import Language.VHDL
import Language.Embedded.VHDL.Expression.Hoist hiding (Kind)

--------------------------------------------------------------------------------
-- * Expressions (and its layers)
--------------------------------------------------------------------------------

relation :: RelationalOperator -> ShiftExpression -> ShiftExpression -> Relation
relation r a b = Relation a (Just (r, b))

shiftexp :: ShiftOperator -> SimpleExpression -> SimpleExpression -> ShiftExpression
shiftexp s a b = ShiftExpression a (Just (s, b))

simplexp :: Maybe Sign -> AddingOperator -> [Term] -> SimpleExpression
simplexp s o (a:as) = SimpleExpression s a (fmap ((,) o) as)

term     :: MultiplyingOperator -> [Factor] -> Term
term     o (a:as) = Term a (fmap ((,) o) as)

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
--
-- Meh..

name, string, lit :: String -> Primary
name   = PrimName . NSimple . Ident
string = PrimLit . LitString . SLit
lit    = PrimLit . LitNum . NLitPhysical . PhysicalLiteral Nothing . NSimple . Ident

null :: Primary
null = PrimLit LitNull

aggregate :: [Expression] -> Primary
aggregate = PrimAgg . Aggregate . fmap (ElementAssociation Nothing)

cast :: SubtypeIndication -> Expression -> Primary
cast (SubtypeIndication _ t _) = PrimTCon . TypeConversion t

qualified :: SubtypeIndication -> Expression -> Primary
qualified (SubtypeIndication _ t _) = PrimQual . QualExp t

function :: Identifier -> [Expression] -> Primary
function i [] = PrimFun $ FunctionCall (NSimple i) Nothing
function i xs = PrimFun
  . FunctionCall (NSimple i)
  . Just
  . AssociationList
  $ fmap (AssociationElement Nothing . APDesignator . ADExpression) xs

--------------------------------------------------------------------------------
-- * Array things
--------------------------------------------------------------------------------

downto :: Expression -> Expression -> Range
downto f t = RSimple (lift f) DownTo (lift t)

downtoZero :: Expression -> Range
downtoZero = flip downto (lift $ lit $ show 0)

upto :: Expression -> Expression -> Range
upto f t = RSimple (lift f) To (lift t)

fromZero :: Expression -> Range
fromZero = upto (lift $ lit $ show 0)

--------------------------------------------------------------------------------

slice :: Identifier -> (SimpleExpression, SimpleExpression) -> Primary
slice i (f, t) = PrimName $ NSlice $ SliceName (PName $ NSimple i) (DRRange $ RSimple f DownTo t)

index :: Identifier -> Expression -> Name
index i l = NIndex $ IndexedName (PName $ NSimple i) [l]

update :: Integral i => Identifier -> i -> Expression -> Primary
update i l e = undefined

--------------------------------------------------------------------------------
-- * Record things
--------------------------------------------------------------------------------

aggregate' :: [(Maybe Choices, Expression)] -> Primary
aggregate' es = PrimAgg $ Aggregate $ map (uncurry ElementAssociation) es

selected  :: Identifier -> Identifier -> Primary
selected p s = PrimName $ NSelect $ SelectedName (PName $ NSimple p) (SSimple s)

--------------------------------------------------------------------------------
