{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- used for the Ord/Eq inst. of XDeclaration etc.
{-# LANGUAGE StandaloneDeriving #-}

module Language.Embedded.VHDL.Monad (
    Kind(..)
  , Type(..)
  , VHDL
    
    -- ^ run
  , runVHDL
  , behavioural
  , structural

    -- ^ assignment
    -- port  / generic  / local
  , constant, constantG, constantL, constantP
  , signal,   signalG,   signalL
  , variable, variableG, variableL, variableP
  , file,     fileG,     fileL,     fileP

    -- ^ ..
  , addSignalAssignment
  , insertProc
  , enterProc

    -- ^ expressions
  , and, or, xor, xnor
  , eq, neq, lt, lte, gt, gte
  , nand, nor
  , sll, srl, sla, sra, rol, ror
  , add, sub, cat
  , neg
  , mul, div, mod, rem
  , exp
  , abs, not
         
  , name, string, lit, null

  , resize

    -- ^ types
  , std_logic
  , signed,  signed8,  signed16,  signed32,  signed64
  , usigned, usigned8, usigned16, usigned32, usigned64
  ) where

import Language.VHDL
import Language.Embedded.VHDL.Expression.Hoist hiding (Kind)

import Control.Applicative hiding (empty)
import Control.Monad
import Control.Monad.State hiding (lift)

import Data.Char     (toLower)
import Data.Foldable (toList)
import Data.List     (find)

import Data.Map      (Map)
import Data.Set      (Set)
import Data.Sequence (Seq, (|>))
import qualified Data.Map      as Map
import qualified Data.Set      as Set
import qualified Data.Sequence as Seq

import Prelude hiding (not, and, or, div, mod, rem, exp, abs, null)
import qualified Prelude as P

--------------------------------------------------------------------------------
-- *
--------------------------------------------------------------------------------

-- | The different kinds of signals/variables/.. that exist in VHDL
data Kind = Constant | Signal | Variable | File

-- | Type indication for signals/variables/..
type Type = SubtypeIndication

--------------------------------------------------------------------------------
-- * VHDL Generation Monad
--------------------------------------------------------------------------------

data EntityState = Entity {
    entity_ident             :: String
  , entity_generics          :: Maybe GenericClause      -- Set InterfaceElement
  , entity_ports             :: Maybe PortClause         -- Set InterfaceElement
  , entity_declarative       :: Seq EntityDeclarativeItem
  , entity_statements        :: Seq EntityStatement
  }

data ProcessState = Process {
    process_current          :: Label
  , process_all              :: Map Label ProcessStatement
  }

data ArchitectureState = Architecture {
    architecture_ident       :: String
  , architecture_header      :: EntityState
  , architecture_declarative :: ArchitectureDeclarativePart
  , architecture_statements  :: Seq ConcurrentStatement
  , architecture_processes   :: ProcessState
  }

newtype VHDL a = VHDL { unVHDL :: State ArchitectureState a}
  deriving ( Functor
           , Applicative
           , Monad
           , MonadState ArchitectureState )

--------------------------------------------------------------------------------
-- ** Run

runVHDL :: ArchitectureState -> VHDL a -> (EntityDeclaration, ArchitectureBody)
runVHDL s m =
    ( EntityDeclaration (Ident ei) (EntityHeader eg ep) (toList ed) (toList' es)
    , ArchitectureBody  (Ident ai) (NSimple (Ident ei)) (toList ad) (toList as))
  where
    (Architecture ai (Entity ei eg ep ed es) ad as _) = execState (unVHDL m) s

    toList' xs
      | P.null xs = Nothing
      | otherwise = Just $ toList xs

emptyArchitectureState :: String -> String -> ArchitectureState
emptyArchitectureState i n =
  Architecture i
    (Entity n Nothing Nothing Seq.empty Seq.empty)
    [] Seq.empty
    (Process (error "label not set") Map.empty)

behavioural, structural :: String -> ArchitectureState
behavioural = emptyArchitectureState "behavioural"
structural  = emptyArchitectureState "structural"

--------------------------------------------------------------------------------
-- ** Gen. Declaration

-- | Creates a new interface declaration from the given attributes
newDeclaration :: Identifier -> Kind -> Maybe Mode -> Type -> Maybe Expression -> InterfaceDeclaration
newDeclaration ident kind mode typ exp = case kind of
  Constant -> InterfaceConstantDeclaration [ident]      typ       exp
  Signal   -> InterfaceSignalDeclaration   [ident] mode typ False exp
  Variable -> InterfaceVariableDeclaration [ident] mode typ       exp
  File     -> InterfaceFileDeclaration     [ident]      typ

-- | Adds a new interface declaration to a list of existing ones
addDeclaration :: InterfaceDeclaration -> InterfaceList -> InterfaceList
addDeclaration new (InterfaceList is) = InterfaceList (insert new is)
  where
    insert :: InterfaceElement -> [InterfaceElement] -> [InterfaceElement]
    insert new [] = [new]
    insert new (i:is)
      | i ~=~ new  = i `add` new : is
      | otherwise  = i           : insert new is

    (~=~)  :: InterfaceElement -> InterfaceElement -> Bool
    (~=~) l r = l { idecl_identifier_list = [] } == r { idecl_identifier_list = [] }

    add    :: InterfaceElement -> InterfaceElement -> InterfaceElement
    add old new = let [n] = idecl_identifier_list new in
      old { idecl_identifier_list = n : idecl_identifier_list old }

--------------------------------------------------------------------------------

addPort :: Identifier -> Kind -> Maybe Mode -> Type -> Maybe Expression -> VHDL Identifier
addPort ident kind mode typ exp =
  do state <- get
     let port  = newDeclaration ident kind mode typ exp
         ports = case entity_ports (architecture_header state) of
           Nothing              -> PortClause (InterfaceList [port])
           Just (PortClause is) -> PortClause (addDeclaration port is)
     put    $ state { architecture_header = (architecture_header state) { entity_ports = Just ports } }
     return $ ident

addGeneric :: Identifier -> Kind -> Maybe Mode -> Type -> Maybe Expression -> VHDL Identifier
addGeneric ident kind mode typ exp =
  do state <- get
     let gen  = newDeclaration ident kind mode typ exp
         gens = case entity_generics (architecture_header state) of
           Nothing                 -> GenericClause (InterfaceList [gen])
           Just (GenericClause is) -> GenericClause (addDeclaration gen is)
     put    $ state { architecture_header = (architecture_header state) { entity_generics = Just gens } }
     return $ ident

--------------------------------------------------------------------------------

newLocalDeclaration :: Identifier -> Kind -> Type -> Maybe Expression -> BlockDeclarativeItem
newLocalDeclaration ident kind typ exp = case kind of
  Constant -> BDIConstant $ ConstantDeclaration       [ident] typ         exp
  Signal   -> BDISignal   $ SignalDeclaration         [ident] typ Nothing exp
  Variable -> BDIShared   $ VariableDeclaration False [ident] typ         exp
  File     -> BDIFile     $ FileDeclaration           [ident] typ file
    where file = case exp of
            Nothing -> Nothing
            Just e  -> Just $ FileOpenInformation Nothing e

addLocalDeclaration :: BlockDeclarativeItem -> ArchitectureDeclarativePart -> ArchitectureDeclarativePart
addLocalDeclaration new blocks = insert new blocks
  where
    insert :: BlockDeclarativeItem -> [BlockDeclarativeItem] -> [BlockDeclarativeItem]
    insert new [] = [new]
    insert new (b:bs)
      | b ~=~ new = b `add` new : bs
      | otherwise = b           : insert new bs

    (~=~)  :: BlockDeclarativeItem -> BlockDeclarativeItem -> Bool
    (~=~) l r = mappy (const []) l == mappy (const []) r

    add    :: BlockDeclarativeItem -> BlockDeclarativeItem -> BlockDeclarativeItem
    add l r = mappy (getty r :) l

    mappy :: (IdentifierList -> IdentifierList) -> BlockDeclarativeItem -> BlockDeclarativeItem
    mappy f b = case b of
      (BDIConstant l) -> BDIConstant $ l {const_identifier_list  = f (const_identifier_list l)}
      (BDISignal l)   -> BDISignal   $ l {signal_identifier_list = f (signal_identifier_list l)}
      (BDIShared l)   -> BDIShared   $ l {var_identifier_list    = f (var_identifier_list l)}
      (BDIFile l)     -> BDIFile     $ l {fd_identifier_list     = f (fd_identifier_list l)}

    getty :: BlockDeclarativeItem -> Identifier
    getty b = case b of
      (BDIConstant l) -> head $ const_identifier_list l
      (BDISignal l)   -> head $ signal_identifier_list l
      (BDIShared l)   -> head $ var_identifier_list l
      (BDIFile l)     -> head $ fd_identifier_list l

--------------------------------------------------------------------------------

addLocal  ::  Identifier -> Kind -> Type -> Maybe Expression -> VHDL Identifier
addLocal ident kind typ exp =
  do state <- get
     let local  = newLocalDeclaration ident kind typ exp
         locals = addLocalDeclaration local $ architecture_declarative state
     put    $ state { architecture_declarative = locals }
     return $ ident

--------------------------------------------------------------------------------

constant, constantG, constantL :: Identifier -> Type -> Maybe Expression -> VHDL Identifier
constant  i typ exp = addPort    i Constant Nothing typ exp
constantG i typ exp = addGeneric i Constant Nothing typ exp
constantL i typ exp = addLocal   i Constant         typ exp

signal, signalG, signalL :: Identifier -> Mode -> Type -> Maybe Expression -> VHDL Identifier
signal  i mod typ exp = addPort    i Signal (Just mod) typ exp
signalG i mod typ exp = addGeneric i Signal (Just mod) typ exp
signalL i mod typ exp = addLocal   i Signal            typ exp

variable, variableG, variableL :: Identifier -> Mode -> Type -> Maybe Expression -> VHDL Identifier
variable  i mod typ exp = addPort    i Variable (Just mod) typ exp
variableG i mod typ exp = addGeneric i Variable (Just mod) typ exp
variableL i mod typ exp = addLocal   i Variable            typ exp

file, fileG, fileL :: Identifier -> Type -> VHDL Identifier
file  i typ = addPort    i File Nothing typ Nothing
fileG i typ = addGeneric i File Nothing typ Nothing
fileL i typ = addLocal   i File         typ Nothing

--------------------------------------------------------------------------------
-- ** Gen. Body

addStatement  :: ConcurrentStatement -> VHDL ()
addStatement s = modify $ \state -> state {architecture_statements = architecture_statements state |> s}

--------------------------------------------------------------------------------
-- **

enterProc  :: Label -> VHDL ()
enterProc l =
  do state <- get
     let processes = architecture_processes state
     put $ state { architecture_processes = processes { process_current = l }}

modifyProc :: (ProcessStatement -> ProcessStatement) -> VHDL ()
modifyProc f =
  do state <- get 
     let processes = architecture_processes state
         current   = process_current processes
         new       = Map.adjust f current $ process_all processes
     put $ state { architecture_processes = processes { process_all = new } }

insertProc :: Label -> ProcessStatement -> VHDL ()
insertProc l ps =
  do state <- get
     let processes = architecture_processes state
         new       = Map.insert l ps $ process_all processes
     put $ state { architecture_processes = processes { process_all = new } }

insertEmptyProc :: Label -> VHDL ()
insertEmptyProc l = insertProc l $ ProcessStatement (Just l) False Nothing [] []

--------------------------------------------------------------------------------
-- *** ...

newProcDeclaration :: Identifier -> Kind -> Type -> Maybe Expression -> ProcessDeclarativeItem
newProcDeclaration ident kind typ exp = case kind of
  Constant -> PDIConstant $ ConstantDeclaration       [ident] typ         exp
  Variable -> PDIVariable $ VariableDeclaration False [ident] typ         exp
  File     -> PDIFile     $ FileDeclaration           [ident] typ file
    where file = case exp of
            Nothing -> Nothing
            Just e  -> Just $ FileOpenInformation Nothing e

addProcDeclaration :: ProcessDeclarativeItem -> ProcessDeclarativePart -> ProcessDeclarativePart
addProcDeclaration new blocks = insert new blocks
  where
    insert :: ProcessDeclarativeItem -> [ProcessDeclarativeItem] -> [ProcessDeclarativeItem]
    insert new [] = [new]
    insert new (b:bs)
      | b ~=~ new = b `add` new : bs
      | otherwise = b           : insert new bs

    (~=~)  :: ProcessDeclarativeItem -> ProcessDeclarativeItem -> Bool
    (~=~) l r = mappy (const []) l == mappy (const []) r

    add    :: ProcessDeclarativeItem -> ProcessDeclarativeItem -> ProcessDeclarativeItem
    add l r = mappy (getty r :) l

    mappy :: (IdentifierList -> IdentifierList) -> ProcessDeclarativeItem -> ProcessDeclarativeItem
    mappy f b = case b of
      (PDIConstant l) -> PDIConstant $ l {const_identifier_list  = f (const_identifier_list l)}
      (PDIVariable l) -> PDIVariable $ l {var_identifier_list    = f (var_identifier_list l)}
      (PDIFile l)     -> PDIFile     $ l {fd_identifier_list     = f (fd_identifier_list l)}

    getty :: ProcessDeclarativeItem -> Identifier
    getty b = case b of
      (PDIConstant l) -> head $ const_identifier_list l
      (PDIVariable l) -> head $ var_identifier_list l
      (PDIFile l)     -> head $ fd_identifier_list l

--------------------------------------------------------------------------------


addProcessLocal :: Identifier -> Kind -> Type -> Maybe Expression -> VHDL Identifier
addProcessLocal ident kind typ exp =
  do state <- gets architecture_processes
     let local  = newProcDeclaration ident kind typ exp
         locals = addProcDeclaration local
     modifyProc $ \s -> s { procs_declarative_part = locals (procs_declarative_part s) }
     return ident

--------------------------------------------------------------------------------

constantP :: Identifier -> Type -> Maybe Expression -> VHDL Identifier
constantP i typ exp = addProcessLocal i Constant typ exp

variableP :: Identifier -> Mode -> Type -> Maybe Expression -> VHDL Identifier
variableP i mod typ exp = addProcessLocal i Variable typ exp

fileP :: Identifier -> Type -> VHDL Identifier
fileP i typ = addProcessLocal i File typ Nothing

--------------------------------------------------------------------------------
-- *** ...

addSignalAssignment :: Identifier -> Expression -> VHDL ()
addSignalAssignment i e = addStatement $
  ConSignalAss
    (CSASCond
      (Nothing)
      (False)
      (ConditionalSignalAssignment
        (TargetName (NSimple i))
        (Options False Nothing)
        (ConditionalWaveforms
          ([])
          ( (WaveElem [WaveEExp e Nothing])
          , (Nothing)))))


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

and, or, xor, xnor :: [Relation] -> Expression
and  = EAnd  
or   = EOr   
xor  = EXor  
xnor = EXnor 

nand, nor :: Relation -> Relation -> Expression
nand a b = ENand a (Just b)
nor  a b = ENor  a (Just b)

eq, neq, lt, lte, gt, gte :: ShiftExpression -> ShiftExpression -> Relation
eq  = relation Eq
neq = relation Neq
lt  = relation Lt
lte = relation Lte
gt  = relation Gt
gte = relation Gte

sll, srl, sla, sra, rol, ror :: SimpleExpression -> SimpleExpression -> ShiftExpression
sll = shiftexp Sll
srl = shiftexp Srl
sla = shiftexp Sla
sra = shiftexp Sra
rol = shiftexp Rol
ror = shiftexp Ror

-- ! These should be ... :: a -> a -> .. and then merge, instead of cheating with [a]
-- ! [a] cannot be empty
add, sub, cat :: [Term] -> SimpleExpression
add = simplexp Nothing Plus
sub = simplexp Nothing Minus
cat = simplexp Nothing Concat

neg :: Term -> SimpleExpression
neg = simplexp (Just Negation) Plus . (: [])

-- ! Same problems as add, sub, ...
mul, div, mod, rem :: [Factor] -> Term
mul = term Times
div = term Div
mod = term Mod
rem = term Rem

exp :: Primary -> Primary -> Factor
exp a b = FacPrim a (Just b)

abs, not :: Primary -> Factor
abs = FacAbs
not = FacNot

-- ! Simplified, names can be more than just strings
name :: String -> Primary
name = PrimName . NSimple . Ident

-- ! Same as name ...
string :: String -> Primary
string = PrimLit . LitString . SLit

lit :: String -> Primary
lit = PrimLit . LitNum . NLitPhysical . PhysicalLiteral Nothing . NSimple . Ident

null  :: Primary
null  = PrimLit LitNull

--------------------------------------------------------------------------------
-- Hack to try things out

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
-- ** Gen. Types

std_logic :: Type
std_logic = SubtypeIndication Nothing (TMType (NSimple (Ident "std_logic"))) Nothing

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
-- Ord instance for use in sequence
--------------------------------------------------------------------------------

instance Ord BlockDeclarativeItem
  where
    compare (BDIConstant l) (BDIConstant r) = compare l r
    compare (BDISignal l)   (BDISignal r)   = compare l r
    compare (BDIShared l)   (BDIShared r)   = compare l r
    compare (BDIFile l)     (BDIFile r)     = compare l r
    compare l               r               = compare (ordBlock l) (ordBlock r)
      where
        ordBlock :: BlockDeclarativeItem -> Int
        ordBlock block = case block of
          (BDIConstant r) -> 1
          (BDISignal r)   -> 2
          (BDIShared r)   -> 3
          (BDIFile r)     -> 4

instance Ord ConstantDeclaration
  where
    compare l r = compare (const_identifier_list l) (const_identifier_list r)

instance Ord SignalDeclaration
  where
    compare l r = compare (signal_identifier_list l) (signal_identifier_list r)

instance Ord VariableDeclaration
  where
    compare l r = compare (var_shared l) (var_shared r)

instance Ord FileDeclaration
  where
    compare l r = compare (fd_identifier_list l) (fd_identifier_list r)

deriving instance Ord Identifier

--------------------------------------------------------------------------------
