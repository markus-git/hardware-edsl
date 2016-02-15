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
  , runVHDLT, runVHDL, execVHDLT, execVHDL

    -- ^ pretty printing
  , prettyVHDL, prettyVHDLT

    -- ^ name generation
  , freshUnique, newSym, newLabel

    -- ^ imports
  , newLibrary, newImport

    -- ^ declarations
  , addPort,       addGeneric
  , addGlobal,     addLocal
  , addConcurrent, addSequential
  , addType,       addComponent

    -- ^ statements
  , inProcess, inFor, inWhile, inConditional, inCase
  , exit

    -- ^ structures
  , entity, architecture, package

    -- ^ common things
  , interfaceConstant, interfaceSignal, interfaceVariable
  , declareConstant,   declareSignal,   declareVariable
  , assignSignal,      assignSignalS,   assignVariable,   assignArray

  , unconstrainedArray, constrainedArray
  ) where

import Language.VHDL

import Control.Applicative    ((<$>))
import Control.Monad.Identity (Identity)
import Control.Monad.State    (StateT, MonadState, MonadIO)
import qualified Control.Monad.Identity as CMI
import qualified Control.Monad.State    as CMS

import Data.Maybe    (catMaybes)
import Data.Foldable (toList)
import Data.Functor  (fmap)
import Data.List     (groupBy)
import Data.Set      (Set)
import Data.Map      (Map)
import qualified Data.Set as Set
import qualified Data.Map as Map

import Text.PrettyPrint (Doc)
import qualified Text.PrettyPrint as Text

import Prelude hiding (null, not, abs, exp, rem, mod, div, and, or)
import qualified Prelude as P

--------------------------------------------------------------------------------
-- * VHDL monad and environment.
--------------------------------------------------------------------------------

-- | Code generation state
data VHDLEnv = VHDLEnv
  { _unique        :: !Integer
  , _designs       :: [DesignFile]
  , _units         :: [DesignUnit]
  , _context       :: Set ContextItem
  , _types         :: Set TypeDeclaration
  , _ports         :: [InterfaceDeclaration]
  , _generics      :: [InterfaceDeclaration]
  , _components    :: Set ComponentDeclaration
  , _global        :: [BlockDeclarativeItem]
  , _local         :: [BlockDeclarativeItem]
  , _concurrent    :: [ConcurrentStatement]
  , _sequential    :: [SequentialStatement]
  }

-- | Initial state during code generation
emptyVHDLEnv = VHDLEnv
  { _unique        = 0
  , _designs       = []
  , _units         = []
  , _context       = Set.empty
  , _types         = Set.empty
  , _components    = Set.empty
  , _ports         = []
  , _generics      = []
  , _global        = []
  , _local         = []
  , _concurrent    = []
  , _sequential    = []
  }

--------------------------------------------------------------------------------
-- * VHDL monad.

-- | Type constraints for the VHDL monads
type MonadV m = (Functor m, Applicative m, Monad m, MonadState VHDLEnv m)

-- | VHDL code generation monad
type VHDL = VHDLT Identity

-- | VHDL code genreation monad transformer.
newtype VHDLT m a = VHDLT { unVGenT :: StateT VHDLEnv m a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadState VHDLEnv
           , MonadIO
           )

-- | Run the VHDL code generation monad transformer.
runVHDLT :: Monad m => VHDLT m a -> VHDLEnv -> m (a, VHDLEnv)
runVHDLT m = CMS.runStateT (unVGenT m)

-- | -- | Executes the VHDL code generation monad transformer, returning only its final state.
execVHDLT :: Monad m => VHDLT m a -> VHDLEnv -> m VHDLEnv
execVHDLT m = CMS.execStateT (unVGenT m)

-- | Run the VHDL code generation monad.
runVHDL :: VHDL a -> VHDLEnv -> (a, VHDLEnv)
runVHDL m = CMI.runIdentity . runVHDLT m

-- | Executes the VHDL code generation monad, returning only its final state.
execVHDL :: VHDL a -> VHDLEnv -> VHDLEnv
execVHDL m = CMI.runIdentity . execVHDLT m

--------------------------------------------------------------------------------
-- ** Generating uniques.

-- | Generates a unique integer.
freshUnique :: MonadV m => m Integer
freshUnique =
  do u <- CMS.gets _unique
     CMS.modify (\e -> e { _unique = u + 1 })
     return u

-- | Generates a fresh and unique identifier.
newSym :: MonadV m => m Identifier
newSym = do i <- freshUnique; return (Ident $ 'v' : show i)

-- | Generates a fresh and unique label.
newLabel :: MonadV m => m Label
newLabel = do i <- freshUnique; return (Ident $ 'l' : show i)

--------------------------------------------------------------------------------
-- ** VHDL environment updates.

-- | Adds a new library import to the context.
newLibrary :: MonadV m => String -> m ()
newLibrary l = CMS.modify $ \s -> s { _context = Set.insert item (_context s) }
  where
    item :: ContextItem
    item = ContextLibrary (LibraryClause (LogicalNameList [Ident l]))

-- | Adds a new library use clause to the context (with an .ALL suffix by default).
newImport :: MonadV m => String -> m ()
newImport i = CMS.modify $ \s -> s { _context = Set.insert item (_context s) }
  where
    item :: ContextItem
    item = ContextUse (UseClause [SelectedName (PName (NSimple (Ident i))) (SAll)])

-- | Adds a port declaration to the entity.
addPort :: MonadV m => InterfaceDeclaration -> m ()
addPort p = CMS.modify $ \s -> s { _ports = p : (_ports s) }

-- | Adds a generic declaration to the entity.
addGeneric :: MonadV m => InterfaceDeclaration -> m ()
addGeneric g = CMS.modify $ \s -> s { _generics = g : (_generics s) }

-- | Adds a type declaration.
addType :: MonadV m => TypeDeclaration -> m ()
addType t = CMS.modify $ \s -> s { _types = Set.insert t (_types s) }

-- | Adds a component declaration.
addComponent :: MonadV m => ComponentDeclaration -> m ()
addComponent c = CMS.modify $ \s -> s { _components = Set.insert c (_components s) }

-- | Adds a global declaration.
addGlobal :: MonadV m => BlockDeclarativeItem -> m ()
addGlobal g = CMS.modify $ \s -> s { _global = g : (_global s) }

-- | Adds a local declaration.
addLocal :: MonadV m => BlockDeclarativeItem -> m ()
addLocal l = CMS.modify $ \s -> s { _local = l : (_local s) }

-- | Adds a concurrent statement.
addConcurrent :: MonadV m => ConcurrentStatement -> m ()
addConcurrent con = CMS.modify $ \s -> s { _concurrent = con : (_concurrent s) }

-- | Adds a sequential statement.
addSequential :: MonadV m => SequentialStatement -> m ()
addSequential seq = CMS.modify $ \s -> s { _sequential = seq : (_sequential s) }

--------------------------------------------------------------------------------
-- * Concurrent and sequential statements
--------------------------------------------------------------------------------

-- | Runs the given action inside a process.
inProcess :: MonadV m => Label -> [Identifier] -> m a -> m (a, ProcessStatement)
inProcess l is m =
  do oldLocals     <- CMS.gets _local
     oldSequential <- CMS.gets _sequential
     CMS.modify $ \e -> e { _local      = []
                          , _sequential = [] }
     result        <- m
     newLocals     <- reverse <$> CMS.gets _local
     newSequential <- reverse <$> CMS.gets _sequential
     CMS.modify $ \e -> e { _local      = oldLocals
                          , _sequential = oldSequential }
     return ( result
            , ProcessStatement
                (Just l)                        -- label
                (False)                         -- postponed
                (sensitivity)                   -- sensitivitylist
                (translate $ merge $ newLocals) -- declarativepart
                (newSequential))                -- statementpart
  where
    sensitivity | P.null is = Nothing
                | otherwise = Just $ SensitivityList $ fmap NSimple is

-- | Run program in for loop.
inFor :: MonadV m => Identifier -> Range -> m () -> m (LoopStatement)
inFor i r m =
  do oldSequential <- CMS.gets _sequential
     CMS.modify $ \e -> e { _sequential = [] }
     m
     newSequential <- reverse <$> CMS.gets _sequential
     CMS.modify $ \e -> e { _sequential = oldSequential }
     return $
       LoopStatement
         (Nothing)
         (Just (IterFor (ParameterSpecification
           (i)
           (DRRange r))))
         (newSequential)

-- | Run program inside while loop.
inWhile :: MonadV m => Label -> Maybe Expression -> m () -> m (LoopStatement)
inWhile l cont m =
  do oldSequential <- CMS.gets _sequential
     CMS.modify $ \e -> e { _sequential = [] }
     m
     newSequential <- reverse <$> CMS.gets _sequential
     CMS.modify $ \e -> e { _sequential = oldSequential }
     return $ 
       LoopStatement
         (Just l)
         (iter cont)
         (newSequential)
  where
    iter :: Maybe Expression -> Maybe IterationScheme
    iter = maybe (Nothing) (Just . IterWhile)

-- | Exit loop.
exit :: MonadV m => Label -> Expression -> m ()
exit label e = addSequential $ SExit $ ExitStatement (Nothing) (Just label) (Just e)

-- | Conditional statements.
inConditional :: MonadV m => (Condition, m ()) -> [(Condition, m ())] -> m () -> m (IfStatement)
inConditional (c, m) os e =
  do let (cs, ns) = unzip os
     oldSequential <- CMS.gets _sequential
     CMS.modify $ \e -> e { _sequential = [] }
     m'  <- contain m
     ns' <- mapM contain ns
     e'  <- contain e
     CMS.modify $ \e -> e { _sequential = oldSequential }
     return $
       IfStatement
         (Nothing)
         (c, m')
         (zip cs ns')
         (maybeList e')
  where
    maybeList :: [SequentialStatement] -> Maybe [SequentialStatement]
    maybeList xs
      | P.null xs = Nothing
      | otherwise = Just xs

-- | Case statements.
inCase :: MonadV m => Expression -> [(Choices, m ())] -> m (CaseStatement)
inCase e choices =
  do let (cs, ns) = unzip choices
     oldSequential <- CMS.gets _sequential
     CMS.modify $ \e -> e { _sequential = [] }
     ns' <- mapM contain ns
     CMS.modify $ \e -> e { _sequential = oldSequential }     
     return $
       CaseStatement
         (Nothing)
         (e)
         (zipWith CaseStatementAlternative cs ns')

contain :: MonadV m => m () -> m [SequentialStatement]
contain m = do
  m                                          -- do
  new <- reverse <$> CMS.gets _sequential    -- get
  CMS.modify $ \e -> e { _sequential = [] }  -- reset
  return new                                 -- return

--------------------------------------------------------------------------------
-- * Design units
--------------------------------------------------------------------------------

-- | Design file.
addDesign :: MonadV m => DesignFile -> m ()
addDesign d = CMS.modify $ \s -> s { _designs = d : (_designs s) }

-- | Design unit with context.
addUnit :: MonadV m => LibraryUnit -> m ()
addUnit lib =
  do ctxt <- CMS.gets _context
     dsig <- CMS.gets _units
     let item = DesignUnit (ContextClause (Set.toList ctxt)) lib
     CMS.modify $ \s -> s { _units   = item : dsig
                          , _context = Set.empty
                          }

-- | Design unit ignoring context.
addUnit_ :: MonadV m => LibraryUnit -> m ()
addUnit_ lib = CMS.modify $ \s -> s { _units = (DesignUnit (ContextClause []) lib) : (_units s)}

--------------------------------------------------------------------------------
-- ** Architectures

-- | Wraps the given monadic action in an architecture, consuming all global
--   identifiers and concurrent statements it produces. Strings are its entity
--   and architecture names, respectively.
architecture :: MonadV m => Identifier -> Identifier -> m a -> m a
architecture entity name m =
  do oldGlobal     <- CMS.gets _global
     oldConcurrent <- CMS.gets _concurrent
     CMS.modify $ \e -> e { _global     = []
                          , _concurrent = [] }
     result        <- m
     newGlobal     <- reverse <$> CMS.gets _global
     newConcurrent <- reverse <$> CMS.gets _concurrent
     addUnit_ $ LibrarySecondary $ SecondaryArchitecture $
           ArchitectureBody
             (name)
             (NSimple entity)
             (merge newGlobal)
             (newConcurrent)
     CMS.modify $ \e -> e { _global     = oldGlobal
                          , _concurrent = oldConcurrent }
     return result

--------------------------------------------------------------------------------
-- ** Entities

-- | Declares an entity with the given name by consuming all port-level
--   declaraions and context items produced by running the monadic action.
entity :: MonadV m => Identifier -> m a -> m a
entity name m =
  do oldPorts    <- CMS.gets _ports
     oldGenerics <- CMS.gets _generics
     CMS.modify $ \e -> e { _ports    = []
                          , _generics = [] }
     result      <- m
     newPorts    <- reverse <$> CMS.gets _ports
     newGenerics <- reverse <$> CMS.gets _generics
     addUnit  $ LibraryPrimary $ PrimaryEntity $
           EntityDeclaration
             (name)
             (EntityHeader
               (GenericClause <$> maybeNull newGenerics)
               (PortClause    <$> maybeNull newPorts))
             ([])
             (Nothing)
     CMS.modify $ \e -> e { _ports    = oldPorts
                          , _generics = oldGenerics }
     return result
  where
    maybeNull :: [InterfaceDeclaration] -> Maybe InterfaceList
    maybeNull [] = Nothing
    maybeNull xs = Just $ InterfaceList $ merge xs

--------------------------------------------------------------------------------
-- ** Packages

-- | Declares a package with the given name by consuming all type declarations
--   produced by running the monadic action.
package :: MonadV m => String -> m a -> m a
package name m =
  do oldTypes <- CMS.gets _types
     CMS.modify $ \e -> e { _types = Set.empty }
     result   <- m
     newTypes <- CMS.gets _types
     addUnit  $ LibraryPrimary $ PrimaryPackage $
           PackageDeclaration
             (Ident name)
             (fmap PHDIType $ Set.toList newTypes)
     CMS.modify $ \e -> e { _types = oldTypes }
     return result

--------------------------------------------------------------------------------
-- ** Component.

-- | Declares an entire component, with entity declaration and a body.
component :: MonadV m => Identifier -> m () -> m () -> m ()
component (Ident name) e a =
  do oldEnv <- CMS.get
     CMS.put emptyVHDLEnv
     entity       en    e
     architecture en an a
     units <- CMS.gets _units
     CMS.put oldEnv
     addDesign $ DesignFile units
  where
    en = Ident $ "entity_"      ++ name
    an = Ident $ "behavioural_" ++ name

--------------------------------------------------------------------------------
-- * Pretty printing VHDL programs
--------------------------------------------------------------------------------

-- | Runs the VHDL monad and pretty prints its resulting VHDL program.
prettyVHDL :: VHDL a -> Doc
prettyVHDL = CMI.runIdentity . prettyVHDLT

-- | Runs the VHDL monad transformer and pretty prints its resulting VHDL program.
prettyVHDLT :: Monad m => VHDLT m a -> m Doc
prettyVHDLT m = prettyVEnv <$> execVHDLT m emptyVHDLEnv

--------------------------------------------------------------------------------

-- | Pretty print a VHDL environment.
prettyVEnv :: VHDLEnv -> Doc
prettyVEnv env = Text.vcat $ pp main : (fmap pp $ _designs env)
  where
    main  = DesignFile $ types ++ archi
    archi = reverse $ _units env
    types = reverse $ designTypes (_types env)

-- *** Scan type declarations for necessary imports instead.
-- *** Types are added in an ugly manner.
designTypes :: Set TypeDeclaration -> [DesignUnit]
designTypes set
  | Set.null set = []
  | otherwise    = _units . snd $ runVHDL pack emptyVHDLEnv
  where
    pack :: MonadV m => m ()
    pack = package "types" $ do
      -- *** Instead of importing every library possible the correct ones
      --     should be declared when adding a type through 'addType'.
      newLibrary "IEEE"
      newImport  "IEEE.STD_LOGIC_1164"
      newImport  "IEEE.NUMERIC_STD"
      CMS.modify $ \e -> e { _types = set }

--------------------------------------------------------------------------------
-- * Common things
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- ** Ports/Generic declarations

interfaceConstant :: Identifier -> SubtypeIndication -> Maybe Expression -> InterfaceDeclaration
interfaceConstant i t e = InterfaceConstantDeclaration [i] t e

interfaceSignal   :: Identifier -> Mode -> SubtypeIndication -> Maybe Expression -> InterfaceDeclaration
interfaceSignal i m t e = InterfaceSignalDeclaration [i] (Just m) t False e

interfaceVariable :: Identifier -> Mode -> SubtypeIndication -> Maybe Expression -> InterfaceDeclaration
interfaceVariable i m t e = InterfaceVariableDeclaration [i] (Just m) t e

--------------------------------------------------------------------------------
-- ** Array Declarations.

compositeTypeDeclaration :: Identifier -> CompositeTypeDefinition -> TypeDeclaration
compositeTypeDeclaration name t = TDFull (FullTypeDeclaration name (TDComposite t))

unconstrainedArray :: Identifier -> SubtypeIndication -> TypeDeclaration
unconstrainedArray name typ = compositeTypeDeclaration name $
  CTDArray (ArrU (UnconstrainedArrayDefinition [] typ))

constrainedArray :: Identifier -> SubtypeIndication -> Range -> TypeDeclaration
constrainedArray name typ range = compositeTypeDeclaration name $
  CTDArray (ArrC (ConstrainedArrayDefinition
    (IndexConstraint [DRRange range]) typ))

--------------------------------------------------------------------------------
-- ** Global/Local Declarations.

declareConstant :: Identifier -> SubtypeIndication -> Maybe Expression -> BlockDeclarativeItem
declareConstant i t e = BDIConstant $ ConstantDeclaration [i] t e

declareSignal :: Identifier -> SubtypeIndication -> Maybe Expression -> BlockDeclarativeItem
declareSignal i t e = BDISignal $ SignalDeclaration [i] t Nothing e

declareVariable :: Identifier -> SubtypeIndication -> Maybe Expression -> BlockDeclarativeItem
declareVariable i t e = BDIShared $ VariableDeclaration False [i] t e

--------------------------------------------------------------------------------
-- ** Assign Signal/Variable.

assignSignal :: Identifier -> Expression -> ConcurrentStatement
assignSignal i e = ConSignalAss $ CSASCond Nothing False $ 
    ConditionalSignalAssignment
      (TargetName (NSimple i))
      (Options False Nothing)
      (ConditionalWaveforms
        ([])
        ( WaveElem [WaveEExp e Nothing]
        , Nothing))

assignSignalS :: Identifier -> Expression -> SequentialStatement
assignSignalS i e = SSignalAss $
  SignalAssignmentStatement
    (Nothing)
    (TargetName (NSimple i))
    (Nothing)
    (WaveElem [WaveEExp e Nothing])

assignVariable :: Identifier -> Expression -> SequentialStatement
assignVariable i e = SVarAss $
  VariableAssignmentStatement
    (Nothing)
    (TargetName (NSimple i))
    (e)

assignArray :: Name -> Expression -> SequentialStatement
assignArray i e = SSignalAss $
  SignalAssignmentStatement
    (Nothing)
    (TargetName i)
    (Nothing)
    (WaveElem [WaveEExp e Nothing])

--------------------------------------------------------------------------------
-- Some helper classes and their instances
--------------------------------------------------------------------------------
--
-- I use BlockDeclarativeItem to represent all declarative items, which means we
-- have to translate them over to their correct VHDL kind when generating an AST

class Merge a
  where
    -- group two items if this holds
    group  :: a -> a -> Bool

    -- merge in this way
    reduce :: [a] -> a

    merge :: [a] -> [a]
    merge = fmap reduce . groupBy group

instance Merge BlockDeclarativeItem
  where
    group  l r      = setBlockIds l [] == setBlockIds r []
    reduce bs@(b:_) = setBlockIds b $ concatMap getBlockIds bs

instance Merge InterfaceDeclaration
  where
    group  l r    = l { idecl_identifier_list = [] } == r { idecl_identifier_list = [] }
    reduce (x:xs) = x { idecl_identifier_list = ids x ++ concatMap ids xs }
      where ids   = idecl_identifier_list

--------------------------------------------------------------------------------

setBlockIds :: BlockDeclarativeItem -> [Identifier] -> BlockDeclarativeItem
setBlockIds (BDIConstant c) is = BDIConstant $ c { const_identifier_list  = is }
setBlockIds (BDISignal   s) is = BDISignal   $ s { signal_identifier_list = is }
setBlockIds (BDIShared   v) is = BDIShared   $ v { var_identifier_list    = is }
setBlockIds (BDIFile     f) is = BDIFile     $ f { fd_identifier_list     = is }
setBlockIds x                 _  = x

getBlockIds :: BlockDeclarativeItem -> [Identifier]
getBlockIds (BDIConstant c) = const_identifier_list c
getBlockIds (BDISignal   s) = signal_identifier_list s
getBlockIds (BDIShared   v) = var_identifier_list v
getBlockIds (BDIFile     f) = fd_identifier_list f

class Declarative a
  where
    -- lists are used so we can fail without having to throw errors
    translate :: [BlockDeclarativeItem] -> [a]

instance Declarative ProcessDeclarativeItem
  where
    translate = catMaybes . fmap tryProcess

-- | Try to transform the declarative item into a process item
tryProcess :: BlockDeclarativeItem -> Maybe (ProcessDeclarativeItem)
tryProcess (BDIConstant c) = Just $ PDIConstant c
tryProcess (BDIShared   v) = Just $ PDIVariable v
tryProcess (BDIFile     f) = Just $ PDIFile     f
tryProcess _                 = Nothing

--------------------------------------------------------------------------------
-- Ord instance for use in sets
--------------------------------------------------------------------------------
--
-- *** These break the Ord rules but seems to be needed for Set.

deriving instance Ord ContextItem
deriving instance Ord LibraryClause
deriving instance Ord LogicalNameList
deriving instance Ord UseClause

instance Ord TypeDeclaration 
  where
    compare (TDFull l)    (TDFull r)    = compare (ftd_identifier l) (ftd_identifier r)
    compare (TDPartial l) (TDPartial r) = compare l r
    compare (TDFull l)    _               = GT
    compare (TDPartial l) _               = LT

deriving instance Ord IncompleteTypeDeclaration

instance Ord ComponentDeclaration
  where
    compare l r = compare (comp_identifier l) (comp_identifier r)

deriving instance Ord SubtypeIndication
deriving instance Ord TypeMark

instance Ord Constraint
  where
    compare (CRange a) (CRange b) = compare a b
    compare _ _ = error "Ord not supported for index constraints"

deriving instance Ord RangeConstraint

instance Ord Range
  where
    compare (RSimple a b c) (RSimple x y z) =
      case compare a x of
        GT -> GT
        LT -> LT
        EQ -> case compare b y of
          GT -> GT
          LT -> LT
          EQ -> case compare c z of
            GT -> GT
            LT -> LT
            EQ -> EQ
    compare _ _ = error "Ord not supported for attribute ranges"

deriving instance Ord Direction
deriving instance Ord Expression
deriving instance Ord Relation
deriving instance Ord ShiftExpression
deriving instance Ord SimpleExpression
deriving instance Ord Term
deriving instance Ord Factor

instance Ord Primary
  where
    compare (PrimName a) (PrimName x) = compare a x

deriving instance Ord LogicalOperator
deriving instance Ord RelationalOperator
deriving instance Ord ShiftOperator
deriving instance Ord AddingOperator
deriving instance Ord Sign
deriving instance Ord MultiplyingOperator
deriving instance Ord MiscellaneousOperator
deriving instance Ord Identifier

instance Ord Name
  where
    compare (NSimple a) (NSimple x) = compare a x
    compare (NSelect a) (NSelect x) = compare a x
    compare (NIndex  a) (NIndex  x) = compare a x
    compare (NSlice  a) (NSlice  x) = compare a x
    compare (NAttr   a) (NAttr   x) = compare a x

deriving instance Ord StringLiteral
deriving instance Ord SelectedName

instance Ord Suffix
  where
    compare (SSimple a) (SSimple x) = compare a x
    compare (SChar   a) (SChar   x) = compare a x
    compare (SAll)      (SAll)      = EQ
    compare _ _ = error "Ord not supported for operator symbols"

deriving instance Ord CharacterLiteral
deriving instance Ord IndexedName
deriving instance Ord SliceName
deriving instance Ord DiscreteRange

instance Ord Prefix
  where
    compare (PName a) (PName x) = compare a x
    compare _ _ = error "Ord not supported for function names"

deriving instance Ord AttributeName
deriving instance Ord Signature

--------------------------------------------------------------------------------
