{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE UndecidableInstances  #-}

module Language.Embedded.VHDL.Monad where

import Language.VHDL

import Control.Applicative
import Control.Monad
import Control.Monad.State hiding (lift)

import Data.Char (toLower)
import Data.List (find)

import Unsafe.Coerce -- !!!

import Prelude hiding (not, and, or)
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
  , entity_generics          :: Maybe GenericClause
  , entity_ports             :: Maybe PortClause
  , entity_declarative       :: EntityDeclarativePart
  , entity_statements        :: Maybe EntityStatementPart
  }

data ArchitectureState = Architecture {
    architecture_ident       :: String
  , architecture_header      :: EntityState
  , architecture_declarative :: ArchitectureDeclarativePart
  , architecture_statements  :: ArchitectureStatementPart
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
    ( EntityDeclaration (Ident ei) (EntityHeader eg ep) ed es
    , ArchitectureBody  (Ident ai) (NSimple (Ident ei)) ad as )
  where
    (Architecture ai (Entity ei eg ep ed es) ad as) = execState (unVHDL m) s

emptyArchitectureState :: String -> String -> ArchitectureState
emptyArchitectureState i n = Architecture i (Entity n Nothing Nothing [] Nothing) [] []

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

newLocalDeclaration :: Identifier -> Kind -> Type -> Maybe Expression -> BlockDeclarativeItem
newLocalDeclaration ident kind typ exp = case kind of
  Constant -> BDIConstantDecl $ ConstantDeclaration       [ident] typ         exp
  Signal   -> BDISignalDecl   $ SignalDeclaration         [ident] typ Nothing exp
  Variable -> BDISharedDecl   $ VariableDeclaration False [ident] typ         exp
  File     -> BDIFileDecl     $ FileDeclaration           [ident] typ file
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
      (BDIConstantDecl l) -> BDIConstantDecl $ l {const_identifier_list  = f (const_identifier_list l)}
      (BDISignalDecl l)   -> BDISignalDecl   $ l {signal_identifier_list = f (signal_identifier_list l)}
      (BDISharedDecl l)   -> BDISharedDecl   $ l {var_identifier_list    = f (var_identifier_list l)}
      (BDIFileDecl l)     -> BDIFileDecl     $ l {fd_identifier_list     = f (fd_identifier_list l)}

    getty :: BlockDeclarativeItem -> Identifier
    getty b = case b of
      (BDIConstantDecl l) -> head $ const_identifier_list l
      (BDISignalDecl l)   -> head $ signal_identifier_list l
      (BDISharedDecl l)   -> head $ var_identifier_list l
      (BDIFileDecl l)     -> head $ fd_identifier_list l

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

addLocal  ::  Identifier -> Kind -> Type -> Maybe Expression -> VHDL Identifier
addLocal ident kind typ exp =
  do state <- get
     let local  = newLocalDeclaration ident kind typ exp
         locals = addLocalDeclaration local $ architecture_declarative state
     put    $ state { architecture_declarative = locals }
     return $ ident

--------------------------------------------------------------------------------

constant, constantG, constantL :: String -> Type -> Maybe Expression -> VHDL Identifier
constant  str typ exp = addPort    (Ident str) Constant Nothing typ exp
constantG str typ exp = addGeneric (Ident str) Constant Nothing typ exp
constantL str typ exp = addLocal   (Ident str) Constant         typ exp

signal, signalG, signalL :: String -> Mode -> Type -> Maybe Expression -> VHDL Identifier
signal  str mod typ exp = addPort    (Ident str) Signal (Just mod) typ exp
signalG str mod typ exp = addGeneric (Ident str) Signal (Just mod) typ exp
signalL str mod typ exp = addLocal   (Ident str) Signal            typ exp

variable, variableG, variableL :: String -> Mode -> Type -> Maybe Expression -> VHDL Identifier
variable  str mod typ exp = addPort    (Ident str) Variable (Just mod) typ exp
variableG str mod typ exp = addGeneric (Ident str) Variable (Just mod) typ exp
variableL str mod typ exp = addLocal   (Ident str) Variable            typ exp

file, fileG, fileL :: String -> Type -> VHDL Identifier
file  str typ = addPort    (Ident str) File Nothing typ Nothing
fileG str typ = addGeneric (Ident str) File Nothing typ Nothing
fileL str typ = addLocal   (Ident str) File         typ Nothing

--------------------------------------------------------------------------------
-- ** Gen. Body

addStatement  :: ConcurrentStatement -> VHDL ()
addStatement s = modify $ \state -> state {architecture_statements = s : architecture_statements state}

addAssignment :: Identifier -> Expression -> VHDL ()
addAssignment i e = addStatement $
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
-- ** Gen. Expression - I'm not sure if this is the dumbest thing ever or not

-- | Lift one level
class Hoist a
  where
    type Next a :: *
    hoist :: a -> Next a

instance Hoist Primary
  where
    type Next Primary = Factor
    hoist p = FacPrim p Nothing

instance Hoist Factor
  where
    type Next Factor = Term
    hoist f = Term f []

instance Hoist Term
  where
    type Next Term = SimpleExpression
    hoist t = SimpleExpression Nothing t []

instance Hoist SimpleExpression
  where
    type Next SimpleExpression = ShiftExpression
    hoist s = ShiftExpression s Nothing

instance Hoist ShiftExpression
  where
    type Next ShiftExpression = Relation
    hoist s = Relation s Nothing

instance Hoist Relation
  where
    type Next Relation = Expression
    hoist r = ENand r Nothing

instance Hoist Expression
  where
    type Next Expression = Primary
    hoist e = PrimExp e

--------------------------------------------------------------------------------

-- | Lift any level
class Lift a b where
  lift :: a -> b

instance Lift a a where
  lift = id

instance (Hoist a, Lift (Next a) b) => Lift a b where
  lift = lift . hoist

--------------------------------------------------------------------------------

-- ...

--------------------------------------------------------------------------------
-- Hmm..
--------------------------------------------------------------------------------

instance Eq BlockDeclarativeItem
  where
    (==) (BDIConstantDecl l) (BDIConstantDecl r) = l == r
    (==) (BDISignalDecl l)   (BDISignalDecl r)   = l == r
    (==) (BDISharedDecl l)   (BDISharedDecl r)   = l == r
    (==) (BDIFileDecl l)     (BDIFileDecl r)     = l == r
    (==) _                   _                   = False

deriving instance Eq InterfaceDeclaration

deriving instance Eq ConstantDeclaration

deriving instance Eq SignalDeclaration

deriving instance Eq VariableDeclaration

deriving instance Eq FileDeclaration

deriving instance Eq Mode

deriving instance Eq SignalKind

deriving instance Eq FileOpenInformation
  
deriving instance Eq SubtypeIndication

deriving instance Eq TypeMark
 
deriving instance Eq Identifier

instance Eq Expression -- todo
  where
    _ == _ = True
  
instance Eq Constraint  -- todo
  where
    _ == _ = False

instance Eq Name -- todo
  where
    NSimple n1 == NSimple n2 = n1 == n2
    _          == _          = False 

--------------------------------------------------------------------------------
