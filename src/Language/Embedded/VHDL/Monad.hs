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
  , DeclarativeItem
    
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
  , addGlobal
  , addLocal

    -- ^ statements
  , addConcurrent
  , addSequential
    
  , inArchitecture
  , inProcess
  , inConditional

    -- ^ common
  , interfaceConstant
  , interfaceSignal
  , interfaceVariable
  , declConstant
  , declSignal
  , declVariable
  , assignSignal
  , assignVariable

  , module Language.Embedded.VHDL.Monad.Type
  , module Language.Embedded.VHDL.Monad.Expression
  ) where

import Language.VHDL ( Identifier(..)
                     , Mode(..)
                     , Expression
                     )
import qualified Language.VHDL as V

import Language.Embedded.VHDL.Monad.Type
import Language.Embedded.VHDL.Monad.Expression

import Control.Arrow          (first, second)
import Control.Applicative
import Control.Monad
import Control.Monad.Identity (Identity)
import Control.Monad.State    (StateT, MonadState, MonadIO)
import qualified Control.Monad.Identity as CMI
import qualified Control.Monad.State    as CMS

import Data.Foldable    (toList)
import Data.Functor
import Data.List        (groupBy)
import Text.PrettyPrint (Doc, ($+$))

import Prelude hiding (null, not, abs, exp, rem, mod, div, and, or)
import qualified Prelude as P

--------------------------------------------------------------------------------
-- * ..
--------------------------------------------------------------------------------

-- A (soon-to-be) superset of VHDL's declaration/assignment/statement types,
-- specific versions (Block../Process../etc) only supports subsets of these
data DeclarativeItem =
    DC V.ConstantDeclaration
  | DS V.SignalDeclaration
  | DV V.VariableDeclaration
  | DF V.FileDeclaration

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
  , _global        :: [DeclarativeItem]
  , _local         :: [DeclarativeItem]

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
  , _global        = []
  , _local         = []
  , _concurrent    = []
  , _sequential    = []
  }

--------------------------------------------------------------------------------

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
-- ** Item declarations

-- | Adds a global declaration
addGlobal :: MonadV m => DeclarativeItem -> m ()
addGlobal g = CMS.modify $ \s -> s { _global = g : (_global s) }

-- | Adds a local declaration
addLocal :: MonadV m => DeclarativeItem -> m ()
addLocal l = CMS.modify $ \s -> s { _local = l : (_local s) }

--------------------------------------------------------------------------------
-- ** Concurrent statements

-- | Runs the given action inside a process
inProcess :: MonadV m => V.Label -> [Identifier] -> m a -> m (a, V.ProcessStatement)
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
                (Just l)                                   -- label
                (False)                                     -- postponed
                (sensitivity)                               -- sensitivitylist
                (fmap decl $ groupDeclarations $ newLocals) -- declarativepart
                (newSequential))                            -- statementpart
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
    contain :: MonadV m => m () -> m [V.SequentialStatement]
    contain m =
      do m                                          -- do
         new <- reverse <$> CMS.gets _sequential    -- get
         CMS.modify $ \e -> e { _sequential = [] }  -- reset
         return $ new

    maybeList :: [a] -> Maybe [a]
    maybeList xs
      | P.null xs = Just xs
      | otherwise = Nothing

--------------------------------------------------------------------------------
-- **

addArchitecture :: MonadV m => V.ArchitectureBody -> m ()
addArchitecture a = CMS.modify $ \s -> s { _architectures = a : (_architectures s)}

inArchitecture :: MonadV m => String -> m a -> m a
inArchitecture name m =
  do oldEntity     <- CMS.gets _entity
     oldGlobal     <- CMS.gets _global
     oldLocal      <- CMS.gets _local
     oldConcurrent <- CMS.gets _concurrent
     CMS.modify $ \e -> e { _global     = []
                          , _local      = []
                          , _concurrent = []
                          }
     a <- m
     newGlobal     <- reverse <$> CMS.gets _global
     newLocal      <- reverse <$> CMS.gets _local
     newConcurrent <- reverse <$> CMS.gets _concurrent
     addArchitecture $ V.ArchitectureBody
       (V.Ident name)
       (V.NSimple (V.Ident oldEntity))
       (fmap decl $ groupDeclarations $ newGlobal ++ newLocal)
       (newConcurrent)
     CMS.modify $ \e -> e { _global     = oldGlobal
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
prettyVEnv (VHDLEnv _ name port generic architecture _ _ _ _) =
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
    maybeNull xs = Just $ V.InterfaceList $ groupInterfaces xs
    
--------------------------------------------------------------------------------
-- Some helper functions
--------------------------------------------------------------------------------

-- | Groups the names of similar declarations
groupDeclarations :: [DeclarativeItem] -> [DeclarativeItem]
groupDeclarations = fmap merge . groupBy (~=~)
  where
    (~=~) :: DeclarativeItem -> DeclarativeItem -> Bool
    (~=~) (DC l) (DC r) = l { V.const_identifier_list  = [] } == r { V.const_identifier_list  = [] }
    (~=~) (DS l) (DS r) = l { V.signal_identifier_list = [] } == r { V.signal_identifier_list = [] }
    (~=~) (DV l) (DV r) = l { V.var_identifier_list    = [] } == r { V.var_identifier_list    = [] }
    (~=~) (DF l) (DF r) = l { V.fd_identifier_list     = [] } == r { V.fd_identifier_list     = [] }
    (~=~) _      _      = False

    merge :: [DeclarativeItem] -> DeclarativeItem
    merge (o@(DC c):cs) = DC $ c { V.const_identifier_list  = cids o ++ concatMap cids cs}
    merge (o@(DS s):ss) = DS $ s { V.signal_identifier_list = sids o ++ concatMap sids ss}
    merge (o@(DV v):vs) = DV $ v { V.var_identifier_list    = vids o ++ concatMap vids vs}
    merge (o@(DF f):fs) = DF $ f { V.fd_identifier_list     = fids o ++ concatMap fids fs}

    cids (DC c) = V.const_identifier_list c
    sids (DS s) = V.signal_identifier_list s
    vids (DV v) = V.var_identifier_list v
    fids (DF f) = V.fd_identifier_list f

groupInterfaces :: [V.InterfaceDeclaration] -> [V.InterfaceDeclaration]
groupInterfaces = fmap merge . groupBy (~=~)
  where
    (~=~) :: V.InterfaceDeclaration -> V.InterfaceDeclaration -> Bool
    (~=~) l r = l { V.idecl_identifier_list = [] } == r { V.idecl_identifier_list = [] }

    merge :: [V.InterfaceDeclaration] -> V.InterfaceDeclaration
    merge (x:xs) = x { V.idecl_identifier_list = ids x ++ concatMap ids xs }
      where
        ids = V.idecl_identifier_list

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

declConstant :: Identifier -> Type -> Maybe Expression -> DeclarativeItem
declConstant i t e = DC $ V.ConstantDeclaration [i] t e

declSignal :: Identifier -> Type -> Maybe Expression -> DeclarativeItem
declSignal i t e = DS $ V.SignalDeclaration [i] t Nothing e

declVariable :: Identifier -> Type -> Maybe Expression -> DeclarativeItem
declVariable i t e = DV $ V.VariableDeclaration False [i] t e

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
-- ! I should replace these.. 

class Declarative a
  where
    decl :: DeclarativeItem -> a

instance Declarative V.ProcessDeclarativeItem
  where
    decl (DC c) = V.PDIConstant c
    decl (DV v) = V.PDIVariable v
    decl (DF f) = V.PDIFile f
    decl _      = error "processes: unsupported declaration"

instance Declarative V.BlockDeclarativeItem
  where
    decl (DC c) = V.BDIConstant c
    decl (DS s) = V.BDISignal s
    decl (DV v) = V.BDIShared v
    decl (DF f) = V.BDIFile f

--------------------------------------------------------------------------------
-- Ord instance for use in Set
--------------------------------------------------------------------------------

deriving instance Ord Identifier

--------------------------------------------------------------------------------
