{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}

-- used for the Ord/Eq inst. of XDeclaration etc.
{-# LANGUAGE StandaloneDeriving #-}

module Language.Embedded.VHDL.Monad (    
    VHDLT
  , VHDL
  , VHDLEnv
  , emptyVHDLEnv
    
    -- ^ run
  , runVHDLT
  , runVHDL

    -- ^ pretty
  , prettyVHDL
  , prettyVHDLT

  , freshUnique
  , newVar

    -- ^ declarations
  , addPort
  , addGeneric
  , addComponent
  , addGlobal
  , addLocal

    -- ^ statements
  , addConcurrent
  , addSequential
    
  , inArchitecture
  , inProcess
  , inConditional
  , inCase

    -- ^ common
  , interfaceConstant
  , interfaceSignal
  , interfaceVariable
  , declConstant
  , declSignal
  , declVariable
  , portMap
  , assignSignal
  , assignVariable

  , module Language.Embedded.VHDL.Monad.Expression
  ) where

import Language.VHDL (Identifier(..), Mode(..), Expression, Label)
import qualified Language.VHDL as V

import Language.Embedded.VHDL.Expression.Type
import Language.Embedded.VHDL.Monad.Expression

import Control.Arrow          (first, second)
import Control.Applicative
import Control.Monad
import Control.Monad.Identity (Identity)
import Control.Monad.State    (StateT, MonadState, MonadIO)
import qualified Control.Monad.Identity as CMI
import qualified Control.Monad.State    as CMS

import Data.Maybe       (catMaybes)
import Data.Foldable    (toList)
import Data.Functor
import Data.List        (groupBy)
import Data.Set         (Set)
import qualified Data.Set as Set

import Text.PrettyPrint (Doc, ($+$))

import Prelude hiding (null, not, abs, exp, rem, mod, div, and, or)
import qualified Prelude as P

--------------------------------------------------------------------------------
-- * ..
--------------------------------------------------------------------------------

-- | Code generation state
data VHDLEnv = VHDLEnv
  { _unique        :: !Integer
  , _entity        :: String

    -- headers
  , _ports         :: [V.InterfaceDeclaration]
  , _generics      :: [V.InterfaceDeclaration]
  , _architectures :: [V.ArchitectureBody]
    
    -- declarations
  , _component     :: Set V.ComponentDeclaration
  , _global        :: [V.BlockDeclarativeItem]
  , _local         :: [V.BlockDeclarativeItem]

    -- statements
  , _concurrent    :: [V.ConcurrentStatement]
  , _sequential    :: [V.SequentialStatement]
  }

-- | Initial state during code generation
emptyVHDLEnv = VHDLEnv
  { _unique        = 0
  , _entity        = "gen"
  , _ports         = []
  , _generics      = []
  , _architectures = []
  , _component     = Set.empty
  , _global        = []
  , _local         = []
  , _concurrent    = []
  , _sequential    = []
  }

--------------------------------------------------------------------------------
-- *

-- | Type constraints for the VHDL monads
type MonadV m = (Functor m, Applicative m, Monad m, MonadState VHDLEnv m)

-- | VHDL code generation monad
type VHDL = VHDLT Identity

-- | VHDL code genreation monad transformer
newtype VHDLT m a = VHDLT { unVGenT :: StateT VHDLEnv m a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadState VHDLEnv
           , MonadIO
           )

-- | Run the VHDL code generation monad transformer
runVHDLT :: Monad m => VHDLT m a -> VHDLEnv -> m (a, VHDLEnv)
runVHDLT m = CMS.runStateT (unVGenT m)

-- | Run the VHDL code generation monad
runVHDL  :: VHDL a -> VHDLEnv -> (a, VHDLEnv)
runVHDL  m = CMI.runIdentity . CMS.runStateT (unVGenT m)

--------------------------------------------------------------------------------
-- ** Unique generation

-- | Generates a unique integer
freshUnique :: MonadV m => m Integer
freshUnique =
  do u <- CMS.gets _unique
     CMS.modify (\e -> e { _unique = u + 1 })
     return u

-- | Generates a fresh and unique identifier
newVar :: MonadV m => m Identifier
newVar = freshUnique >>= return . Ident . ('v' :) . show

--------------------------------------------------------------------------------
-- ** Header declarations -- ignores port/generic maps for now

-- | Adds a port declaration to the entity
addPort :: MonadV m => V.InterfaceDeclaration -> m ()
addPort p = CMS.modify $ \s -> s { _ports = p : (_ports s) }

-- | Adds a generic declaration to the entity
addGeneric :: MonadV m => V.InterfaceDeclaration -> m ()
addGeneric g = CMS.modify $ \s -> s { _generics = g : (_generics s) }

--------------------------------------------------------------------------------
-- ** Component declarations

addComponent :: MonadV m => V.ComponentDeclaration -> m ()
addComponent c = CMS.modify $ \s -> s { _component = Set.insert c (_component s) }

--------------------------------------------------------------------------------
-- ** Item declarations

-- | Adds a global declaration
addGlobal :: MonadV m => V.BlockDeclarativeItem -> m ()
addGlobal g = CMS.modify $ \s -> s { _global = g : (_global s) }

-- | Adds a local declaration
addLocal :: MonadV m => V.BlockDeclarativeItem -> m ()
addLocal l = CMS.modify $ \s -> s { _local = l : (_local s) }

--------------------------------------------------------------------------------
-- ** Concurrent statements

-- | Runs the given action inside a process
-- ! Due to how translate works, some local declarations might dissapear.
inProcess :: MonadV m => Label -> [Identifier] -> m a -> m (a, V.ProcessStatement)
inProcess l is m =
  do oldLocals     <- CMS.gets _local
     oldSequential <- CMS.gets _sequential
     CMS.modify $ \e -> e { _local      = []
                          , _sequential = [] }
     a <- m
     newLocals     <- reverse <$> CMS.gets _local
     newSequential <- reverse <$> CMS.gets _sequential
     CMS.modify $ \e -> e { _local      = oldLocals
                          , _sequential = oldSequential }
     return ( a
            , V.ProcessStatement
                (Just l)                        -- label
                (False)                         -- postponed
                (sensitivity)                   -- sensitivitylist
                (translate $ merge $ newLocals) -- declarativepart
                (newSequential))                -- statementpart
  where
    sensitivity | P.null is = Nothing
                | otherwise = Just $ V.SensitivityList $ fmap V.NSimple is

--------------------------------------------------------------------------------
-- ** Sequential statements

addConcurrent :: MonadV m => V.ConcurrentStatement -> m ()
addConcurrent con = CMS.modify $ \s -> s { _concurrent = con : (_concurrent s) }

addSequential :: MonadV m => V.SequentialStatement -> m ()
addSequential seq = CMS.modify $ \s -> s { _sequential = seq : (_sequential s) }

--------------------------------------------------------------------------------

contain :: MonadV m => m () -> m [V.SequentialStatement]
contain m =
 do m                                          -- do
    new <- reverse <$> CMS.gets _sequential    -- get
    CMS.modify $ \e -> e { _sequential = [] }  -- reset
    return $ new

--------------------------------------------------------------------------------

inConditional :: MonadV m => (V.Condition, m ()) -> [(V.Condition, m ())] -> m () -> m (V.IfStatement)
inConditional (c, m) os e =
  do let (cs, ns) = unzip os
     oldSequential <- CMS.gets _sequential
     CMS.modify $ \e -> e { _sequential = [] }
     m'  <- contain m
     ns' <- mapM contain ns
     e'  <- contain e
     CMS.modify $ \e -> e { _sequential = oldSequential }
     return $
       V.IfStatement
         (Nothing)
         (c, m')
         (zip cs ns')
         (maybeList e')
  where
    maybeList :: [V.SequentialStatement] -> Maybe [V.SequentialStatement]
    maybeList xs
      | P.null xs = Nothing
      | otherwise = Just xs

--------------------------------------------------------------------------------

inCase :: MonadV m => V.Expression -> [(V.Choices, m ())] -> m (V.CaseStatement)
inCase e choices =
  do let (cs, ns) = unzip choices
     oldSequential <- CMS.gets _sequential
     CMS.modify $ \e -> e { _sequential = [] }
     ns' <- mapM contain ns
     CMS.modify $ \e -> e { _sequential = oldSequential }     
     return $
       V.CaseStatement
         (Nothing)
         (e)
         (zipWith V.CaseStatementAlternative cs ns')

--------------------------------------------------------------------------------
-- **

addArchitecture :: MonadV m => V.ArchitectureBody -> m ()
addArchitecture a = CMS.modify $ \s -> s { _architectures = a : (_architectures s)}

inArchitecture :: MonadV m => String -> m a -> m a
inArchitecture name m =
  do oldEntity     <- CMS.gets _entity
     oldComponent  <- CMS.gets _component
     oldGlobal     <- CMS.gets _global
     oldLocal      <- CMS.gets _local
     oldConcurrent <- CMS.gets _concurrent
     CMS.modify $ \e -> e { _component  = Set.empty
                          , _global     = []
                          , _local      = []
                          , _concurrent = []
                          }
     a <- m
     newComponent  <-             CMS.gets _component
     newGlobal     <- reverse <$> CMS.gets _global
     newLocal      <- reverse <$> CMS.gets _local
     newConcurrent <- reverse <$> CMS.gets _concurrent
     let declarations =
           merge $ newGlobal ++ newLocal
     addArchitecture $ V.ArchitectureBody
       (V.Ident name)
       (V.NSimple (V.Ident oldEntity))
       (merge $ newGlobal ++ newLocal)
       (newConcurrent)
     CMS.modify $ \e -> e { _component  = oldComponent
                          , _global     = oldGlobal
                          , _local      = oldLocal
                          , _concurrent = oldConcurrent
                          }
     return a

--------------------------------------------------------------------------------
-- * Pretty
--------------------------------------------------------------------------------

prettyVHDL :: VHDL a -> Doc
prettyVHDL = CMI.runIdentity . prettyVHDLT

prettyVHDLT :: Monad m => VHDLT m a -> m Doc
prettyVHDLT m = prettyVEnv . snd <$> runVHDLT m emptyVHDLEnv

prettyVEnv  :: VHDLEnv -> Doc
prettyVEnv (VHDLEnv _ name port generic architecture _ _ _ _ _) =
    foldr1 ($+$) (V.pp entity : fmap V.pp architecture)
  where
    entity :: V.EntityDeclaration
    entity =
      V.EntityDeclaration
        (V.Ident name)
        (V.EntityHeader
          (V.GenericClause <$> maybeNull generic)
          (V.PortClause    <$> maybeNull port))
        ([])
        (Nothing)

    maybeNull :: [V.InterfaceDeclaration] -> Maybe V.InterfaceList
    maybeNull [] = Nothing
    maybeNull xs = Just $ V.InterfaceList $ merge xs
    
--------------------------------------------------------------------------------
-- * Common things
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- **  Ports/Generic declarations

interfaceConstant :: Identifier -> Type -> Maybe Expression -> V.InterfaceDeclaration
interfaceConstant i t e = V.InterfaceConstantDeclaration [i] t e

interfaceSignal   :: Identifier -> Mode -> Type -> Maybe Expression -> V.InterfaceDeclaration
interfaceSignal i m t e = V.InterfaceSignalDeclaration [i] (Just m) t False e

interfaceVariable :: Identifier -> Mode -> Type -> Maybe Expression -> V.InterfaceDeclaration
interfaceVariable i m t e = V.InterfaceVariableDeclaration [i] (Just m) t e

--------------------------------------------------------------------------------
-- ** Global/Local Declarations

declConstant :: Identifier -> Type -> Maybe Expression -> V.BlockDeclarativeItem
declConstant i t e = V.BDIConstant $ V.ConstantDeclaration [i] t e

declSignal :: Identifier -> Type -> Maybe Expression -> V.BlockDeclarativeItem
declSignal i t e = V.BDISignal $ V.SignalDeclaration [i] t Nothing e

declVariable :: Identifier -> Type -> Maybe Expression -> V.BlockDeclarativeItem
declVariable i t e = V.BDIShared $ V.VariableDeclaration False [i] t e

--------------------------------------------------------------------------------
-- ** Component instantiation (port mapping)

portMap :: Label -> Identifier -> [V.ActualDesignator] -> V.ConcurrentStatement
portMap l n ns = V.ConComponent $
  V.ComponentInstantiationStatement
    (l)
    (V.IUComponent (V.NSimple n))
    (Nothing)
    (Just $ V.PortMapAspect
      (V.AssociationList $
        fmap (V.AssociationElement Nothing) $
          fmap V.APDesignator ns))

--------------------------------------------------------------------------------
-- ** Assign Signal/Variable

assignSignal :: Identifier -> Expression -> V.SequentialStatement
assignSignal i e = V.SSignalAss $
  V.SignalAssignmentStatement
    (Nothing)
    (V.TargetName (V.NSimple i))
    (Nothing)
    (V.WaveElem [V.WaveEExp e Nothing])

assignVariable :: Identifier -> Expression -> V.SequentialStatement
assignVariable i e = V.SVarAss $
  V.VariableAssignmentStatement
    (Nothing)
    (V.TargetName (V.NSimple i))
    (e)

--------------------------------------------------------------------------------
-- Some helper classes and their instances
--------------------------------------------------------------------------------

class Merge a
  where
    -- group two items if this holds
    group  :: a -> a -> Bool

    -- merge in this way
    reduce :: [a] -> a

    merge :: [a] -> [a]
    merge = fmap reduce . groupBy group


instance Merge V.BlockDeclarativeItem
  where
    group  l r      = setBlockIds l [] == setBlockIds r []
    reduce bs@(b:_) = setBlockIds b $ concatMap getBlockIds bs

instance Merge V.InterfaceDeclaration
  where
    group  l r    = l { V.idecl_identifier_list = [] } == r { V.idecl_identifier_list = [] }
    reduce (x:xs) = x { V.idecl_identifier_list = ids x ++ concatMap ids xs }
      where ids   = V.idecl_identifier_list

--------------------------------------------------------------------------------

setBlockIds :: V.BlockDeclarativeItem -> [Identifier] -> V.BlockDeclarativeItem
setBlockIds (V.BDIConstant c) is = V.BDIConstant $ c { V.const_identifier_list  = is }
setBlockIds (V.BDISignal   s) is = V.BDISignal   $ s { V.signal_identifier_list = is }
setBlockIds (V.BDIShared   v) is = V.BDIShared   $ v { V.var_identifier_list    = is }
setBlockIds (V.BDIFile     f) is = V.BDIFile     $ f { V.fd_identifier_list     = is }
setBlockIds x                 _  = x

getBlockIds :: V.BlockDeclarativeItem -> [Identifier]
getBlockIds (V.BDIConstant c) = V.const_identifier_list c
getBlockIds (V.BDISignal   s) = V.signal_identifier_list s
getBlockIds (V.BDIShared   v) = V.var_identifier_list v
getBlockIds (V.BDIFile     f) = V.fd_identifier_list f

--------------------------------------------------------------------------------
-- I use BlockDeclarativeItem to represent all declarative items, which means we
-- have to translate them over to their correct VHDL kind when generating an AST
--------------------------------------------------------------------------------

class Declarative a
  where
    -- lists are used so we can fail without having to throw errors
    translate :: [V.BlockDeclarativeItem] -> [a]

instance Declarative V.ProcessDeclarativeItem
  where
    translate = catMaybes . fmap tryProcess

tryProcess :: V.BlockDeclarativeItem -> Maybe (V.ProcessDeclarativeItem)
tryProcess (V.BDIConstant c) = Just $ V.PDIConstant c
tryProcess (V.BDIShared   v) = Just $ V.PDIVariable v
tryProcess (V.BDIFile     f) = Just $ V.PDIFile     f
tryProcess _                 = Nothing

--------------------------------------------------------------------------------
-- Ord instance for use in Set
--------------------------------------------------------------------------------

deriving instance Ord Identifier

instance Ord V.ComponentDeclaration
  where
    compare l r = compare (V.comp_identifier l) (V.comp_identifier r)


--------------------------------------------------------------------------------
