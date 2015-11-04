{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}

-- used for the Ord/Eq inst. of XDeclaration etc.
{-# LANGUAGE StandaloneDeriving #-}

module Language.Embedded.VHDL.Monad (    
    VHDL
  , VHDLT
  , VHDLEnv
  , emptyVHDLEnv
    
    -- ^ run
  , runVHDLT
  , runVHDL

    -- ^ pretty
  , prettyVHDL
  , prettyVHDLT

    -- ^ ...
  , freshUnique
  , newVar
  , newLabel

    -- ^ declarations
  , addPort
  , addGeneric
  , addType
  , addComponent
  , addDeclaration
  , addGlobal
  , addLocal

    -- ^ statements
  , addConcurrent
  , addSequential

    -- ^ key statements
  , inEntity
  , inArchitecture
  , inProcess
  , inConditional
  , inCase

    -- ^ common
  , interfaceConstant
  , interfaceSignal
  , interfaceVariable
  , declRecord
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

-- | ...
data Entity = Entity (V.EntityDeclaration) [V.ArchitectureBody]

-- | Code generation state
data VHDLEnv = VHDLEnv
  { _unique        :: !Integer
  , _entity        :: String

    -- ..
  , _parts         :: [Entity]
  , _definitions   :: Set V.TypeDeclaration

    -- ...
  , _types         :: Set V.TypeDeclaration
  , _components    :: Set V.ComponentDeclaration

    -- headers
  , _ports         :: [V.InterfaceDeclaration]
  , _generics      :: [V.InterfaceDeclaration]
  , _architectures :: [V.ArchitectureBody]
    
    -- declarations
  , _global        :: [V.BlockDeclarativeItem]
  , _local         :: [V.BlockDeclarativeItem]

    -- statements
  , _concurrent    :: [V.ConcurrentStatement]
  , _sequential    :: [V.SequentialStatement]
  }

-- | Initial state during code generation
emptyVHDLEnv = VHDLEnv
  { _unique        = 0
  , _entity        = "invisible"
  , _parts         = []
  , _definitions   = Set.empty
  , _types         = Set.empty
  , _components    = Set.empty
  , _ports         = []
  , _generics      = []
  , _architectures = []
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
-- ** Generating uniques

-- | Generates a unique integer
freshUnique :: MonadV m => m Integer
freshUnique =
  do u <- CMS.gets _unique
     CMS.modify (\e -> e { _unique = u + 1 })
     return u

-- | Generates a fresh and unique identifier
newVar :: MonadV m => m Identifier
newVar = freshUnique >>= return . Ident . ('u' :) . show

-- | Generates a fresh and unique label
newLabel :: MonadV m => m Label
newLabel = freshUnique >>= return . Ident . ('l' :) . show

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

-- | ...
addType :: MonadV m => V.TypeDeclaration -> m ()
addType t = CMS.modify $ \s -> s { _types = Set.insert t (_types s) }

-- | Adds a component declaration to the architecture
addComponent :: MonadV m => V.ComponentDeclaration -> m ()
addComponent c = CMS.modify $ \s -> s { _components = Set.insert c (_components s) }

-- | ...
addDeclaration :: MonadV m => V.TypeDeclaration -> m ()
addDeclaration d = CMS.modify $ \s -> s { _definitions = Set.insert d (_definitions s) }

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

addConcurrent :: MonadV m => V.ConcurrentStatement -> m ()
addConcurrent con = CMS.modify $ \s -> s { _concurrent = con : (_concurrent s) }

--------------------------------------------------------------------------------
-- *** Process-statements

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

addSequential :: MonadV m => V.SequentialStatement -> m ()
addSequential seq = CMS.modify $ \s -> s { _sequential = seq : (_sequential s) }

--------------------------------------------------------------------------------
-- *** If-statements

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

contain :: MonadV m => m () -> m [V.SequentialStatement]
contain m =
 do m                                          -- do
    new <- reverse <$> CMS.gets _sequential    -- get
    CMS.modify $ \e -> e { _sequential = [] }  -- reset
    return $ new

--------------------------------------------------------------------------------
-- *** Case-statements

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
-- ** Creating architectures

addArchitecture :: MonadV m => V.ArchitectureBody -> m ()
addArchitecture a = CMS.modify $ \s -> s { _architectures = a : (_architectures s)}

inArchitecture :: MonadV m => String -> m a -> m a
inArchitecture name m =
  do oldEntity     <- CMS.gets _entity
     oldComponent  <- CMS.gets _components
     oldGlobal     <- CMS.gets _global
     oldLocal      <- CMS.gets _local
     oldConcurrent <- CMS.gets _concurrent
     CMS.modify $ \e -> e { _components = Set.empty
                          , _global     = []
                          , _local      = []
                          , _concurrent = []
                          }
     a <- m
     newComponent  <-             CMS.gets _components
     newGlobal     <- reverse <$> CMS.gets _global
     newLocal      <- reverse <$> CMS.gets _local
     newConcurrent <- reverse <$> CMS.gets _concurrent
     CMS.modify $ \e -> e { _components = oldComponent
                          , _global     = oldGlobal
                          , _local      = oldLocal
                          , _concurrent = oldConcurrent
                          }

     addArchitecture $ V.ArchitectureBody
       (V.Ident name)
       (V.NSimple (V.Ident oldEntity))
       (merge $ newGlobal ++ newLocal)
       (newConcurrent)
     return a

--------------------------------------------------------------------------------
-- ** Creating sub-entities

addEntity  :: MonadV m => Entity -> m ()
addEntity v = CMS.modify $ \s -> s { _parts = v : (_parts s) }

inEntity :: MonadV m => String -> m a -> m a
inEntity name m =
  do oldUnique        <- CMS.gets _unique
     oldEntity        <- CMS.gets _entity
     oldPorts         <- CMS.gets _ports
     oldGenerics      <- CMS.gets _generics
     oldArchitectures <- CMS.gets _architectures
     CMS.modify $ \e -> e { _unique        = 0
                          , _entity        = name
                          , _ports         = []
                          , _generics      = []
                          , _architectures = []
                          }
     a <- m
     newPorts         <- reverse <$> CMS.gets _ports
     newGenerics      <- reverse <$> CMS.gets _generics
     newArchitectures <- reverse <$> CMS.gets _architectures
     CMS.modify $ \e -> e { _unique        = oldUnique
                          , _entity        = oldEntity
                          , _ports         = oldPorts
                          , _generics      = oldGenerics
                          , _architectures = oldArchitectures
                          }
     addEntity $ Entity
       (V.EntityDeclaration
         (V.Ident name)
         (V.EntityHeader
           (V.GenericClause <$> maybeNull newGenerics)
           (V.PortClause    <$> maybeNull newPorts))
         ([])
         (Nothing))
       (newArchitectures)
     return a
  where
    maybeNull :: [V.InterfaceDeclaration] -> Maybe V.InterfaceList
    maybeNull [] = Nothing
    maybeNull xs = Just $ V.InterfaceList $ merge xs

--------------------------------------------------------------------------------
-- * Pretty
--------------------------------------------------------------------------------

prettyVHDL :: VHDL a -> Doc
prettyVHDL = CMI.runIdentity . prettyVHDLT

prettyVHDLT :: Monad m => VHDLT m a -> m Doc
prettyVHDLT m = prettyVEnv . snd <$> runVHDLT (inEntity "anonymous" m) emptyVHDLEnv

prettyVEnv  :: VHDLEnv -> Doc
prettyVEnv env = stack $
    (genPackage "types" $ Set.toList (_types env)) : (fmap pretty (_parts env))
  where
    stack :: [Doc] -> Doc
    stack = foldr1 ($+$)
    
    pretty :: Entity -> Doc
    pretty (Entity e as) = stack (V.pp e : fmap V.pp as)

--------------------------------------------------------------------------------

genPackage :: String -> [V.TypeDeclaration] -> Doc
genPackage name = V.pp . V.PackageDeclaration (V.Ident name) . fmap V.PHDIType

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
-- ** Type/Component Declarations

declRecord :: Identifier -> [(Identifier, Type)] -> V.TypeDeclaration
declRecord name es = V.TDFull
  (V.FullTypeDeclaration
    (name)
    (V.TDComposite (V.CTDRecord (V.RecordTypeDefinition
      (fmap decl es)
      (Just name)))))
  where
    decl (i, t) = V.ElementDeclaration [i] t

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

-- | Try to transform the declarative item into a process item
tryProcess :: V.BlockDeclarativeItem -> Maybe (V.ProcessDeclarativeItem)
tryProcess (V.BDIConstant c) = Just $ V.PDIConstant c
tryProcess (V.BDIShared   v) = Just $ V.PDIVariable v
tryProcess (V.BDIFile     f) = Just $ V.PDIFile     f
tryProcess _                 = Nothing

--------------------------------------------------------------------------------
-- Ord instance for use in Set
--------------------------------------------------------------------------------

deriving instance Ord Identifier

--------------------------------------------------------------------------------

instance Ord V.TypeDeclaration where
  compare (V.TDFull l)    (V.TDFull r)    = compare (V.ftd_identifier l) (V.ftd_identifier r)
  compare (V.TDPartial l) (V.TDPartial r) = compare l r
  -- ...
  compare (V.TDFull l)    _               = GT
  compare (V.TDPartial l) _               = LT

deriving instance Ord V.IncompleteTypeDeclaration

instance Ord V.ComponentDeclaration where
  compare l r = compare (V.comp_identifier l) (V.comp_identifier r)

--------------------------------------------------------------------------------
