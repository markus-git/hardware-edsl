{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}

-- used for the Ord/Eq inst. of XDeclaration etc.
{-# LANGUAGE StandaloneDeriving #-}

module Language.Embedded.VHDL.Monad (
    VHDL
  , VHDLT
  , VHDLEnv
  , MonadV
  , emptyVHDLEnv
    
    -- ^ run
  , runVHDLT, runVHDL, execVHDLT, execVHDL

    -- ^ pretty printing
  , prettyVHDL, prettyVHDLT

    -- ^ entity container
  , wrapMain
  
    -- ^ name generation
  , freshUnique, newSym, newLabel

    -- ^ imports
  , addLibrary, addImport

    -- ^ ...
  , addPort
  , addConstant,   addSignal,     addVariable
  , addConcurrent, addSequential
  , addType,       addComponent

    -- ^ ...
  , lookupArrayType

    -- ^ ...
  , importComponent
                   
    -- ^ statements
  , inProcess, inFor, inWhile, inConditional, inCase
  , exit, null

    -- ^ specialized statements
  , inSingleProcess

    -- ^ structures
  , entity, architecture, component

    -- ^ common things
  , port, constant, signal, variable, array
  , assignSignal, assignVariable, assignArray
  , concurrentSignal, concurrentArray
  , portMap
  ) where

import Language.VHDL

import Language.Embedded.VHDL.Monad.Expression
  (eq, literal, number, simple, name, function)
import Language.Embedded.VHDL.Monad.Type
  (std_logic, eqType, eqRange)
import Language.Embedded.VHDL.Monad.Util
  (expr, primExpr, primShift, primSimple, primTerm, primFactor, maybePrimary)

import Control.Applicative    ((<$>))
import Control.Monad.Identity (Identity)
import Control.Monad.State    (StateT, MonadState, MonadIO)
import qualified Control.Monad.Identity as CMI
import qualified Control.Monad.State    as CMS

import Data.Char     (isLetter)
import Data.Either   (partitionEithers)
import Data.Maybe    (catMaybes, isJust)
import Data.Foldable (toList)
import Data.Functor  (fmap)
import Data.List     (groupBy, isPrefixOf, stripPrefix, find, partition)
import Data.Set      (Set)
import Data.Map      (Map)
import qualified Data.Set as Set
import qualified Data.Map as Map

import Text.PrettyPrint (Doc)
import qualified Text.PrettyPrint as Text

import Prelude hiding (null, not, abs, exp, rem, mod, div, and, or)
import qualified Prelude as P

import Debug.Trace

--------------------------------------------------------------------------------
-- * VHDL monad and environment.
--------------------------------------------------------------------------------

-- | Code generation state
data VHDLEnv = VHDLEnv
  { _unique        :: !Integer
  , _designs       :: [DesignFile]
  , _units         :: [DesignUnit]
  , _context       :: [ContextItem]
  , _types         :: [TypeDeclaration]
  , _components    :: [ComponentDeclaration]
  , _ports         :: [InterfaceDeclaration]
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
  , _context       = []
  , _types         = []
  , _components    = []
  , _ports         = []
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

-- | VHDL code genreation monad transformer.
newtype VHDLT m a = VHDLT { unVGenT :: StateT VHDLEnv m a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadState VHDLEnv
           , MonadIO
           )

-- | VHDL code generation monad
type VHDL = VHDLT Identity

-- | Run the VHDL code generation monad transformer.
runVHDLT :: Monad m => VHDLT m a -> VHDLEnv -> m (a, VHDLEnv)
runVHDLT m = CMS.runStateT (unVGenT m)

-- | Executes the VHDL code generation monad transformer, returning only its final state.
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

-- | Adds an element to a list if there's no element matching the predicate.
add :: (a -> Bool) -> a -> [a] -> [a]
add p a as
  | isJust $ find p as = as
  | otherwise          = a : as

--------------------------------------------------------------------------------
-- ** VHDL environment updates.

-- | Adds a new library import to the context.
addLibrary :: MonadV m => String -> m ()
addLibrary l = CMS.modify $ \s -> s { _context = add (cmp l) lib (_context s) }
  where
    lib :: ContextItem
    lib = ContextLibrary (LibraryClause (LogicalNameList [Ident l]))

    cmp :: String -> ContextItem -> Bool
    cmp s (ContextLibrary (LibraryClause (LogicalNameList [Ident l]))) = s == l
    cmp _ _ = False

-- | Adds a new library use clause to the context (with an .ALL suffix by default).
addImport :: MonadV m => String -> m ()
addImport i = CMS.modify $ \s -> s { _context = add (cmp i) imp (_context s) }
  where
    imp :: ContextItem
    imp = ContextUse (UseClause [SelectedName (PName (NSimple (Ident i))) SAll])

    cmp :: String -> ContextItem -> Bool
    cmp s (ContextUse (UseClause [SelectedName (PName (NSimple (Ident i))) _])) = s == i
    cmp _ _ = False

-- | Adds a type declaration.
addType :: MonadV m => TypeDeclaration -> m ()
addType t = CMS.modify $ \s -> s { _types = t : _types s }

-- | Adds a component declaration.
addComponent :: MonadV m => ComponentDeclaration -> m ()
addComponent c = CMS.modify $ \s -> s { _components = c : _components s }

-- | Adds a port declaration.
addPort :: MonadV m => InterfaceDeclaration -> m ()
addPort p = CMS.modify $ \s -> s { _ports = p : _ports s }

-- | ...
addConstant :: MonadV m => InterfaceDeclaration -> m ()
addConstant c = CMS.modify $ \s -> s { _constants = c : _constants s }

-- | Adds a global declaration.
addSignal :: MonadV m => InterfaceDeclaration -> m ()
addSignal v = CMS.modify $ \s -> s { _signals = v : _signals s }

-- | Adds a local declaration.
--addVariable :: MonadV m => BlockDeclarativeItem -> m ()
addVariable :: MonadV m => InterfaceDeclaration -> m ()
addVariable v = CMS.modify $ \s -> s { _variables = v : _variables s }

-- | Adds a concurrent statement.
addConcurrent :: MonadV m => ConcurrentStatement -> m ()
addConcurrent con = CMS.modify $ \s -> s { _concurrent = con : _concurrent s }

-- | Adds a sequential statement.
addSequential :: MonadV m => SequentialStatement -> m ()
addSequential seq = CMS.modify $ \s -> s { _sequential = seq : _sequential s }

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

-- | Runs the given action, with its corresponding reset, in a process that
--   triggers on positive clock edges.
inSingleProcess :: MonadV m
  => Label        -- ^ Process label.
  -> Identifier   -- ^ Clock.
  -> Maybe (Identifier, m ())
                  -- ^ Reset and program.
  -> [Identifier] -- ^ Sensitivity list.
  -> m ()         -- ^ Main program.
  -> m ()
inSingleProcess l clk rst is n =
  inProcess' (clk : is) $
    inConditional'
      ( whenRising' clk
      , case rst of
          Just (r, m) -> inConditional' (isLow r, n) m
          Nothing     -> n
      )
      (return ())
  where
    inProcess' :: MonadV m => [Identifier] -> m () -> m ()
    inProcess' is m = inProcess l is m >>= addConcurrent . ConProcess . snd

    inConditional' :: MonadV m => (Condition, m ()) -> m () -> m ()
    inConditional' c e = inConditional c [] e >>= addSequential . SIf

    whenRising' :: Identifier -> Condition
    whenRising' i = expr (function (simple "rising_edge") [expr (name (NSimple i))])

    isLow :: Identifier -> Condition
    isLow i = primExpr $ eq
        (shift' (name (NSimple i)))
        (shift' (literal (number "\'0\'")))
      where
        shift' :: Primary -> ShiftExpression
        shift' = primShift . primSimple . primTerm . primFactor

--------------------------------------------------------------------------------
-- * Design units
--------------------------------------------------------------------------------

-- | Design file.
addDesign :: MonadV m => DesignFile -> m ()
addDesign d = CMS.modify $ \s -> s { _designs = d : (_designs s) }

-- | Design unit with context.
addUnit :: MonadV m => LibraryUnit -> m ()
addUnit lib =
  do dsig <- CMS.gets _units
     ctxt <- CMS.gets _context
     let item = DesignUnit (ContextClause ctxt) lib
     CMS.modify $ \s -> s { _units   = item : dsig
                          , _context = []
                          }

-- | Design unit ignoring context. Used for design units that inherit their
--   context from others, like architectures.
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
     oldComponents <- CMS.gets _components
     CMS.modify $ \e -> e { _constants  = []
                          , _signals    = []
                          , _concurrent = []
                          , _sequential = []
                          , _components = [] }
     result        <- m
     newConstants  <- reverse <$> CMS.gets _constants
     newGlobal     <- reverse <$> CMS.gets _signals
     newConcurrent <- reverse <$> CMS.gets _concurrent
     newSequential <- reverse . filter isSignal <$> CMS.gets _sequential
     newComponents <- fmap BDIComp <$> CMS.gets _components
     let signals   =  fmap translateSequential newSequential
     addUnit_ $ LibrarySecondary $ SecondaryArchitecture $
           ArchitectureBody (name)
             (NSimple entity)
             (newComponents -- ++ newTypes
                ++ fmap translateInterface newGlobal
                ++ fmap translateInterface newConstants)
             (signals ++ newConcurrent)
     CMS.modify $ \e -> e { _constants  = oldConstants
                          , _signals    = oldGlobal
                          , _concurrent = oldConcurrent
                          , _sequential = oldSequential
                          , _components = oldComponents }
--   extendContext entity
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
  do oldPorts    <- CMS.gets _ports
     CMS.modify $ \e -> e { _ports = [] }
     result      <- m
     newPorts    <- reverse <$> CMS.gets _ports
     addUnit $ LibraryPrimary $ PrimaryEntity $ EntityDeclaration name
       (EntityHeader (Nothing) (PortClause <$> maybeNull newPorts)) [] Nothing
     CMS.modify $ \e -> e { _ports = oldPorts }
     return result
  where
    maybeNull :: [InterfaceDeclaration] -> Maybe InterfaceList
    maybeNull [] = Nothing
    maybeNull xs = Just $ InterfaceList xs --merge ...

--------------------------------------------------------------------------------
-- ** Component.

-- | Declares an entire component, with entity declaration and a body.
component :: MonadV m => m a -> m a
component m =
  do oldEnv   <- CMS.get
     oldFiles <- CMS.gets _designs
     oldTypes <- CMS.gets _types
     CMS.put $ emptyVHDLEnv {
         _designs = oldFiles
       , _types   = oldTypes
       }
     result   <- m
     newUnits <- reverse <$> CMS.gets _units
     newFiles <- CMS.gets _designs
     newTypes <- CMS.gets _types
     CMS.put $ oldEnv {
         _designs = newFiles
       , _types   = newTypes
       }
     addDesign $ DesignFile newUnits
     return result
-- todo: since the types carry over ther could be name clashes in the generated
--       array types. This isn't a problem in our examples, but I should fix it.

--------------------------------------------------------------------------------
-- ** Common declarations.

port :: MonadV m => Identifier -> Mode -> SubtypeIndication -> Maybe Expression -> m ()
port i m t e = addPort $ InterfaceSignalDeclaration [i] (Just m) t False e

constant :: MonadV m => Identifier -> SubtypeIndication -> Expression -> m ()
constant i t e = addConstant $ InterfaceConstantDeclaration [i] t (Just e)

signal :: MonadV m => Identifier -> Mode -> SubtypeIndication -> Maybe Expression -> m ()
signal i m t e = addSignal $ InterfaceSignalDeclaration [i] (Just m) t False e

variable :: MonadV m => Identifier -> SubtypeIndication -> Maybe Expression -> m ()
variable i t e = addVariable $ InterfaceVariableDeclaration [i] Nothing t e

array :: MonadV m => Identifier -> Mode -> SubtypeIndication -> Maybe Expression -> m ()
array = signal

--------------------------------------------------------------------------------
-- ** Assign Signal/Variable.

assignSignal :: MonadV m => Name -> Expression -> m ()
assignSignal n e = addSequential $ SSignalAss $ 
  SignalAssignmentStatement Nothing (TargetName n) Nothing (WaveElem [WaveEExp e Nothing])

assignVariable :: MonadV m => Name -> Expression -> m ()
assignVariable n e = addSequential $ SVarAss $
  VariableAssignmentStatement Nothing (TargetName n) e

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

portMap :: MonadV m => Label -> Identifier -> [(Maybe Identifier, Identifier)] -> m ()
portMap l c is = addConcurrent $ ConComponent $ ComponentInstantiationStatement l
  (IUComponent $ NSimple c)
  (Nothing)
  (Just $ PortMapAspect $ AssociationList $ flip fmap is $ \(i, j) ->
    AssociationElement
      (fmap (FPDesignator . FDPort . NSimple) i)
      (APDesignator $ ADSignal $ NSimple j))

importComponent :: MonadV m => Identifier -> [InterfaceDeclaration] -> m ()
importComponent name is = addComponent $ ComponentDeclaration name Nothing
    (Just (PortClause (InterfaceList is)))
    (Nothing)

--------------------------------------------------------------------------------
-- ....

null :: MonadV m => m ()
null = addSequential $ SNull $ NullStatement Nothing

--------------------------------------------------------------------------------
-- * Pretty printing VHDL programs
--------------------------------------------------------------------------------

-- | Runs the VHDL monad and pretty prints its resulting VHDL program.
prettyVHDL :: VHDL a -> Doc
prettyVHDL = CMI.runIdentity . prettyVHDLT

-- | Runs the VHDL monad transformer and pretty prints its resulting VHDL program.
prettyVHDLT :: Monad m => VHDLT m a -> m Doc
prettyVHDLT m = prettyVEnv <$> execVHDLT (m >> package) emptyVHDLEnv
  where
    -- todo: importing like this is a bity "cheaty", as its assumets we know
    --       what kind of types will be packages. Also, I assume there _will_
    --       be a package called \types\ if any arrays are used.
    package :: Monad m => VHDLT m ()
    package =
      do addLibrary "IEEE"
         addImport  "IEEE.std_logic_1164"
         addImport  "IEEE.numeric_std"
         types <- CMS.gets _types
         ctxt  <- CMS.gets _context
--       CMS.when (P.not $ P.null types) $ addUnit $
         addDesign $ DesignFile $ (:[]) $ DesignUnit (ContextClause ctxt) $
           LibraryPrimary $ PrimaryPackage $ PackageDeclaration
             (Ident "types") (fmap PHDIType (reverse types))

--------------------------------------------------------------------------------

-- | Pretty print a VHDL environment.
prettyVEnv :: VHDLEnv -> Doc
prettyVEnv = Text.vcat . map pp . map reorderDesign . _designs

--------------------------------------------------------------------------------

reorderDesign :: DesignFile -> DesignFile
reorderDesign (DesignFile units) = DesignFile $ map reorderUnit $ reverse units

reorderUnit :: DesignUnit -> DesignUnit
reorderUnit (DesignUnit ctxt lib) = DesignUnit (reorderContext ctxt) lib

reorderContext :: ContextClause -> ContextClause
reorderContext (ContextClause items) = ContextClause $ concatMap reorder $ groupBy prefix $ reverse items
  where
    prefix :: ContextItem -> ContextItem -> Bool
    prefix a b = prefixOf a == prefixOf b
      where
        prefixOf :: ContextItem -> String
        prefixOf (ContextLibrary (LibraryClause (LogicalNameList [Ident l])))          = l
        prefixOf (ContextUse (UseClause [SelectedName (PName (NSimple (Ident i))) _])) = takeWhile isLetter i

    reorder :: [ContextItem] -> [ContextItem]
    reorder cs = let (l, u) = partition isLib cs in l ++ u
      where
        isLib :: ContextItem -> Bool
        isLib (ContextLibrary _) = True
        isLib _ = False

--------------------------------------------------------------------------------
-- Some helper functions, classes and their instances
--------------------------------------------------------------------------------

-- | Wraps a program in an entity container.
wrapMain :: MonadV m => m a -> m ()
wrapMain prog = do
  let eName = Ident "main"
  let aName = Ident "behav"
  CMS.void $ component $ entity eName $ do
    port (Ident "clk") (In) (std_logic) (Nothing)
    port (Ident "rst") (In) (std_logic) (Nothing)
    architecture eName aName prog
-- todo: take clock and reset names as parameters?

--------------------------------------------------------------------------------

lookupArrayType :: MonadV m => TypeDeclaration -> m (Maybe Identifier)
lookupArrayType t =
  do types <- CMS.gets _types
     return $ case find (compareTypeDecl t) types of
       Just (TDFull (FullTypeDeclaration i _))  -> Just i
       Nothing -> Nothing
  where
    compareTypeDecl :: TypeDeclaration -> TypeDeclaration -> Bool
    compareTypeDecl
        (TDFull (FullTypeDeclaration _ (TDComposite (CTDArray t1))))
        (TDFull (FullTypeDeclaration _ (TDComposite (CTDArray t2))))
      = compareType t1 t2
    compareTypeDecl _ _ = False

    compareType :: ArrayTypeDefinition -> ArrayTypeDefinition -> Bool
    compareType
      (ArrC (ConstrainedArrayDefinition (IndexConstraint [DRRange r1]) t1))
      (ArrC (ConstrainedArrayDefinition (IndexConstraint [DRRange r2]) t2))
      = eqType t1 t2 && eqRange r1 r2
    compareType _ _ = False

--------------------------------------------------------------------------------
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
