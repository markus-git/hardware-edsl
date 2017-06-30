module Language.Embedded.VHDL.Monad.Expression
  ( and, or, xor, xnor, nand, nor
  , eq, neq, lt, lte, gt, gte
  , sll, srl, sla, sra, rol, ror
  , add, sub, cat, neg
  , mul, div, mod, rem
  , exp, abs, not
  , name, simple, indexed, selected, slice, attribute
  , literal, number, string
  , aggregate, aggregated, associated, others
  , function
  , qualified
  , cast
  -- utility.
  , resize, asBits, asSigned, asUnsigned, toSigned, toUnsigned, toInteger
  , range, downto, to
  , point, upper, lower, zero
  , choices, is, between
  ) where

import Language.VHDL

import Prelude hiding (and, or, div, mod, rem, exp, abs, not, null, toInteger)

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
-- ** Primaries.

--------------------------------------------------------------------------------
-- *** Names.

name :: Name -> Primary
name = PrimName

simple :: String -> Name
simple = NSimple . Ident

selected :: Name -> Identifier -> Name
selected i s = NSelect $ SelectedName (PName i) (SSimple s)

indexed :: Name -> Expression -> Name
indexed i l = NIndex $ IndexedName (PName i) [l]

slice :: Name -> Range -> Name
slice i r = NSlice $ SliceName (PName i) (DRRange r)

attribute :: String -> Name -> Primary
attribute s n = PrimName $ NAttr $ AttributeName (PName n) Nothing (Ident s) Nothing

--------------------------------------------------------------------------------
-- *** Literals.

literal :: Literal -> Primary
literal = PrimLit

number :: String -> Literal
number = LitNum . NLitPhysical . PhysicalLiteral Nothing . NSimple . Ident

string :: String -> Literal
string = LitString . SLit

--------------------------------------------------------------------------------
-- *** Aggregates.

aggregate :: Aggregate -> Primary
aggregate = PrimAgg

aggregated :: [Expression] -> Aggregate
aggregated = Aggregate . fmap (ElementAssociation Nothing)

associated :: [(Maybe Choices, Expression)] -> Aggregate
associated es = Aggregate $ map (uncurry ElementAssociation) es

others :: Expression -> Aggregate
others = Aggregate . (:[]) . ElementAssociation (Just (Choices [ChoiceOthers]))

--------------------------------------------------------------------------------
-- *** Functions.

function :: Name -> [Expression] -> Primary
function i [] = PrimFun $ FunctionCall i Nothing
function i xs = PrimFun
  . FunctionCall i . Just . AssociationList
  $ fmap (AssociationElement Nothing . APDesignator . ADExpression) xs

--------------------------------------------------------------------------------
-- *** Qualified.

qualified :: SubtypeIndication -> Expression -> Primary
qualified (SubtypeIndication _ t _) = PrimQual . QualExp t

--------------------------------------------------------------------------------
-- *** Type conversion.

cast :: SubtypeIndication -> Expression -> Primary
cast (SubtypeIndication _ t _) = PrimTCon . TypeConversion (unrange t)
  where
    unrange :: TypeMark -> TypeMark
    unrange (TMType (NSlice (SliceName (PName (NSimple (Ident typ))) _))) = TMType (NSimple (Ident typ))
    unrange (TMType (NSimple (Ident typ))) = TMType (NSimple (Ident typ))

--------------------------------------------------------------------------------
-- ...

resize :: Expression -> Expression -> Primary
resize exp size = function (simple "resize") [exp, size]

asBits :: Expression -> Primary
asBits exp = function (simple "std_logic_vector") [exp]

asSigned :: Expression -> Primary
asSigned exp = function (simple "signed") [exp]

asUnsigned :: Expression -> Primary
asUnsigned exp = function (simple "unsigned") [exp]

toSigned :: Expression -> Expression -> Primary
toSigned exp size = function (simple "to_signed") [exp, size]

toUnsigned :: Expression -> Expression -> Primary
toUnsigned exp size = function (simple "to_unsigned") [exp, size]

toInteger :: Expression -> Primary
toInteger exp = function (simple "to_integer") [exp]

--------------------------------------------------------------------------------
-- ...

range :: SimpleExpression -> Direction -> SimpleExpression -> Range
range  = RSimple

downto, to :: Direction
downto = DownTo
to     = To

--------------------------------------------------------------------------------
-- ...

point :: Show i => i -> SimpleExpression
point i = SimpleExpression Nothing (Term (FacPrim (literal (number (show i))) (Nothing)) []) []

upper, lower :: Integer -> SimpleExpression
upper 0 = point 0
upper n = point (n-1)
lower n = point n

zero :: SimpleExpression
zero = point 0

--------------------------------------------------------------------------------
-- ...

choices :: [Choice] -> Choices
choices = Choices

is :: SimpleExpression -> Choice
is = ChoiceSimple

between :: Range -> Choice
between = ChoiceRange . DRRange

--------------------------------------------------------------------------------
