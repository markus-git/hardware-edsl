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

    -- ^ ...
--, addPort,       addGeneric
  , addConstant,   addSignal,     addVariable
  , addConcurrent, addSequential
  , addType,       addComponent

    -- ^ declarations
  , declareComponent
                   
    -- ^ statements
  , inProcess, inFor, inWhile, inConditional, inCase
  , exit, null

    -- ^ structures
  , entity, architecture, package, component

    -- ^ common things
  , constant, signal, variable, array, unconstrainedArray, constrainedArray
  , assignSignal, assignVariable, assignArray
  , concurrentSignal, concurrentArray
  , portMap
  ) where

import Language.VHDL

import Control.Applicative    ((<$>))
import Control.Monad.Identity (Identity)
import Control.Monad.State    (StateT, MonadState, MonadIO)
import qualified Control.Monad.Identity as CMI
import qualified Control.Monad.State    as CMS

import Data.Either   (partitionEithers)
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
  , _components    :: Set ComponentDeclaration
  , _constants     :: [InterfaceDeclaration]
  , _signals       :: [InterfaceDeclaration]
  , _variables     :: [InterfaceDeclaration]
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
  , _constants     = []
  , _signals       = []
  , _variables     = []
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
newSym :: MonadV m => String -> m String
newSym n = do i <- freshUnique; return (n ++ show i)

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

-- | Adds a type declaration.
addType :: MonadV m => TypeDeclaration -> m ()
addType t = CMS.modify $ \s -> s { _types = Set.insert t (_types s) }

-- | Adds a component declaration.
addComponent :: MonadV m => ComponentDeclaration -> m ()
addComponent c = CMS.modify $ \s -> s { _components = Set.insert c (_components s) }

-- | ...
addConstant :: MonadV m => InterfaceDeclaration -> m ()
addConstant c = CMS.modify $ \s -> s { _constants = c : (_constants s) }

-- | Adds a global declaration.
addSignal :: MonadV m => InterfaceDeclaration -> m ()
addSignal v = CMS.modify $ \s -> s { _signals = v : (_signals s) }

-- | Adds a local declaration.
--addVariable :: MonadV m => BlockDeclarativeItem -> m ()
addVariable :: MonadV m => InterfaceDeclaration -> m ()
addVariable v = CMS.modify $ \s -> s { _variables = v : (_variables s) }

-- | Adds a concurrent statement.
addConcurrent :: MonadV m => ConcurrentStatement -> m ()
addConcurrent con = CMS.modify $ \s -> s { _concurrent = con : (_concurrent s) }

-- | Adds a sequential statement.
addSequential :: MonadV m => SequentialStatement -> m ()
addSequential seq = CMS.modify $ \s -> s { _sequential = seq : (_sequential s) }

--------------------------------------------------------------------------------
-- * Concurrent and sequential statements
--------------------------------------------------------------------------------

-- | Extract block declaration from interface declaration.
translateInterface :: InterfaceDeclaration -> BlockDeclarativeItem
translateInterface (InterfaceConstantDeclaration is t e) =
  BDIConstant (ConstantDeclaration is t e)
translateInterface (InterfaceSignalDeclaration is m t b e) =
  BDISignal (SignalDeclaration is t (Just (if b then Bus else Register)) e)
translateInterface (InterfaceVariableDeclaration is m t e) =
  BDIShared (VariableDeclaration False is t e)
translateInterface (InterfaceFileDeclaration is t) =
  BDIFile (FileDeclaration is t Nothing)

-- | ...
translateSequential :: SequentialStatement -> ConcurrentStatement
translateSequential (SSignalAss (SignalAssignmentStatement _ name _ e)) =
  ConSignalAss (CSASCond Nothing False (ConditionalSignalAssignment name (Options False Nothing) (ConditionalWaveforms [] (e, Nothing))))

-- | ...
translateConcurrent :: ConcurrentStatement -> SequentialStatement
translateConcurrent (ConSignalAss (CSASCond _ _ (ConditionalSignalAssignment name _ (ConditionalWaveforms _ (e, _))))) =
  SSignalAss (SignalAssignmentStatement Nothing name Nothing e)

-- | Run monadic actions in a contained environment.
contain :: MonadV m => m () -> m [SequentialStatement]
contain m =
  do m                                          -- do
     new <- reverse <$> CMS.gets _sequential    -- get
     CMS.modify $ \e -> e { _sequential = [] }  -- reset
     return new                                 -- return

-- | Exit loop.
exit :: MonadV m => Label -> Expression -> m ()
exit label e = addSequential $ SExit $ ExitStatement (Nothing) (Just label) (Just e)

--------------------------------------------------------------------------------

-- | Runs the given action inside a process.
inProcess :: MonadV m => Label -> [Identifier] -> m a -> m (a, ProcessStatement)
inProcess l is m =
  do oldLocals     <- CMS.gets _variables
     oldSequential <- CMS.gets _sequential
     CMS.modify $ \e -> e { _variables  = []
                          , _sequential = [] }
     result        <- m
     newLocals     <- reverse <$> CMS.gets _variables
     newSequential <- reverse <$> CMS.gets _sequential
     CMS.modify $ \e -> e { _variables  = oldLocals
                          , _sequential = oldSequential }
     return ( result
            , ProcessStatement (Just l) (False)
                (sensitivity is)
                (fmap (translate . translateInterface) $ merge $ newLocals)
                (newSequential))
  where
    sensitivity :: [Identifier] -> Maybe SensitivityList
    sensitivity [] = Nothing
    sensitivity xs = Just $ SensitivityList $ fmap NSimple xs

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
       IfStatement Nothing
         (c, m')
         (zip cs ns')
         (maybeList e')
  where
    maybeList :: [SequentialStatement] -> Maybe [SequentialStatement]
    maybeList [] = Nothing
    maybeList xs = Just xs

-- | Case statements.
inCase :: MonadV m => Expression -> [(Choices, m ())] -> m () -> m (CaseStatement)
inCase e choices d =
  do let (cs, ns) = unzip choices
     oldSequential <- CMS.gets _sequential
     CMS.modify $ \e -> e { _sequential = [] }
     ns' <- mapM contain ns
     d'  <- contain d
     CMS.modify $ \e -> e { _sequential = oldSequential }
     let xs = zipWith CaseStatementAlternative cs ns'
     return $
       CaseStatement Nothing e
         (xs ++ maybeList d')
  where
    maybeList :: [SequentialStatement] -> [CaseStatementAlternative]
    maybeList [] = []
    maybeList xs = [CaseStatementAlternative (Choices [ChoiceOthers]) xs]
    

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
architecture entity@(Ident n) name@(Ident e) m =
  do oldConstants  <- CMS.gets _constants
     oldGlobal     <- CMS.gets _signals
     oldConcurrent <- CMS.gets _concurrent
     oldSequential <- CMS.gets _sequential
     oldTypes      <- CMS.gets _types
     oldComponents <- CMS.gets _components
     CMS.modify $ \e -> e { _constants  = []
                          , _signals    = []
                          , _concurrent = []
                          , _sequential = []
                          , _types      = Set.empty
                          , _components = Set.empty }
     result        <- m
     newConstants  <- reverse <$> CMS.gets _constants
     newGlobal     <- reverse <$> CMS.gets _signals
     newConcurrent <- reverse <$> CMS.gets _concurrent
     newSequential <- reverse . filter isSignal <$> CMS.gets _sequential
     newTypes      <- fmap BDIType . Set.toList <$> CMS.gets _types
     newComponents <- fmap BDIComp . Set.toList <$> CMS.gets _components
     let signals   =  fmap translateSequential newSequential
     addUnit_ $ LibrarySecondary $ SecondaryArchitecture $
           ArchitectureBody (name)
             (NSimple entity)
             (newTypes -- merge
                ++ newComponents
                ++ fmap translateInterface newGlobal
                ++ fmap translateInterface newConstants)
             (signals ++ newConcurrent)
     CMS.modify $ \e -> e { _constants  = oldConstants
                          , _signals    = oldGlobal
                          , _concurrent = oldConcurrent
                          , _sequential = oldSequential
                          , _types      = oldTypes
                          , _components = oldComponents }
     return result
  where
    isSignal :: SequentialStatement -> Bool
    isSignal (SSignalAss _) = True
    isSignal _              = False

--------------------------------------------------------------------------------
-- ** Entities

-- | Declares an entity with the given name by consuming all port-level
--   declaraions and context items produced by running the monadic action.
entity :: MonadV m => Identifier -> m a -> m a
entity name@(Ident n) m =
  do oldPorts    <- CMS.gets _signals
     oldGenerics <- CMS.gets _variables
     CMS.modify $ \e -> e { _signals   = []
                          , _variables = [] }
     result      <- m
     newPorts    <- reverse <$> CMS.gets _signals
     newGenerics <- reverse <$> CMS.gets _variables
     addUnit  $ LibraryPrimary $ PrimaryEntity $
           EntityDeclaration name
             (EntityHeader
               (GenericClause <$> maybeNull newGenerics)
               (PortClause    <$> maybeNull newPorts))
             ([])
             (Nothing)
     CMS.modify $ \e -> e { _signals   = oldPorts
                          , _variables = oldGenerics }
     return result
  
maybeNull :: [InterfaceDeclaration] -> Maybe InterfaceList
maybeNull [] = Nothing
maybeNull xs = Just $ InterfaceList $ xs --merge ...

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
component :: MonadV m => m () -> m ()
component m =
  do oldEnv <- CMS.get
     CMS.put emptyVHDLEnv
     m
     units <- CMS.gets _units
     CMS.put oldEnv
     addDesign $ DesignFile units

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

-- todo: Scan type declarations for necessary imports instead.
--   * aren't we doing this already?
-- todo: Types are added in an ugly manner.
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

constant :: MonadV m => Identifier -> SubtypeIndication -> Expression -> m ()
constant i t e = addConstant $ InterfaceConstantDeclaration [i] t (Just e)

signal :: MonadV m => Identifier -> Mode -> SubtypeIndication -> Maybe Expression -> m ()
signal i m t e = addSignal $ InterfaceSignalDeclaration [i] (Just m) t False e

variable :: MonadV m => Identifier -> SubtypeIndication -> Maybe Expression -> m ()
variable i t e = addVariable $ InterfaceVariableDeclaration [i] Nothing t e

array :: MonadV m => Identifier -> Mode -> SubtypeIndication -> Maybe Expression -> m ()
array = signal

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
-- ** Assign Signal/Variable.

assignSignal :: MonadV m => Name -> Expression -> m ()
assignSignal n e = addSequential $ SSignalAss $ 
  SignalAssignmentStatement
    (Nothing)
    (TargetName n)
    (Nothing)
    (WaveElem [WaveEExp e Nothing])

assignVariable :: MonadV m => Name -> Expression -> m ()
assignVariable n e = addSequential $ SVarAss $
  VariableAssignmentStatement
    (Nothing)
    (TargetName n)
    (e)

assignArray :: MonadV m => Name -> Expression -> m ()
assignArray = assignSignal

--------------------------------------------------------------------------------

concurrentSignal :: MonadV m => Name -> Expression -> m ()
concurrentSignal n e = addConcurrent $ ConSignalAss $
  CSASCond Nothing False $
  ConditionalSignalAssignment (TargetName n) (Options False Nothing) $
  ConditionalWaveforms [] (WaveElem [WaveEExp e Nothing], Nothing)

concurrentArray :: MonadV m => Name -> Expression -> m ()
concurrentArray = concurrentSignal

--------------------------------------------------------------------------------
-- Portmap.

portMap :: MonadV m => Label -> Identifier -> [(Identifier, Identifier)] -> m ()
portMap l c is = addConcurrent $ ConComponent $ ComponentInstantiationStatement l
  (IUComponent $ NSimple c)
  (Nothing)
  (Just $ PortMapAspect $ AssociationList $ flip fmap is $ \(i, j) ->
    AssociationElement (Just $ FPDesignator $ FDPort $ NSimple i) $ APDesignator $ ADSignal $ NSimple j)

declareComponent :: MonadV m => Identifier -> [InterfaceDeclaration] -> m ()
declareComponent name is = addComponent $ ComponentDeclaration name Nothing
  (Just (PortClause (InterfaceList is)))
  (Nothing)

--------------------------------------------------------------------------------
-- ....

null :: MonadV m => m ()
null = addSequential $ SNull $ NullStatement Nothing

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

class Declarative a
  where
    translate :: BlockDeclarativeItem -> a

instance Declarative ProcessDeclarativeItem
  where
    translate = processBlock

-- | Try to transform the declarative item into a process item
processBlock :: BlockDeclarativeItem -> ProcessDeclarativeItem
processBlock (BDIConstant c) = PDIConstant c
processBlock (BDIShared   v) = PDIVariable v
processBlock (BDIFile     f) = PDIFile     f
processBlock b               = error $ "Unknown block item: " ++ show b

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
