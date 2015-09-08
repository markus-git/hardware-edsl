module Language.Embedded.VHDL.Monad.Architecture where

import Language.VHDL
import Language.Embedded.VHDL.Monad.VHDL
import Language.Embedded.VHDL.Monad.Type

import Control.Applicative hiding (empty)
import Control.Monad
import Control.Monad.State hiding (lift)

import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq (empty)

--------------------------------------------------------------------------------
-- * Architecture manipulation
--------------------------------------------------------------------------------

iConstant :: IdentifierList -> Type -> Maybe Expression -> DeclarativeItem
iConstant is t e = C $ ConstantDeclaration is t e

iSignal :: IdentifierList -> Type -> Maybe SignalKind -> Maybe Expression -> DeclarativeItem
iSignal is t k e = S $ SignalDeclaration is t k e

iVariable :: Bool -> IdentifierList -> Type -> Maybe Expression -> DeclarativeItem
iVariable shared is t e = V $ VariableDeclaration shared is t e

iFile :: IdentifierList -> Type -> Maybe FileOpenInformation -> DeclarativeItem
iFile is t fo = F $ FileDeclaration is t fo

fileInfo :: Maybe Expression -> FileLogicalName -> FileOpenInformation
fileInfo = FileOpenInformation

--------------------------------------------------------------------------------
-- **

mapIs :: (IdentifierList -> IdentifierList) -> DeclarativeItem -> DeclarativeItem
mapIs g item = case item of
  (C c) -> C $ c { const_identifier_list  = g (const_identifier_list c)  }
  (S s) -> S $ s { signal_identifier_list = g (signal_identifier_list s) }
  (V v) -> V $ v { var_identifier_list    = g (var_identifier_list v)    }
  (F f) -> F $ f { fd_identifier_list     = g (fd_identifier_list f)     }

getIs :: DeclarativeItem -> IdentifierList
getIs item = case item of
  (C c) -> const_identifier_list c
  (S s) -> signal_identifier_list s
  (V v) -> var_identifier_list v
  (F f) -> fd_identifier_list f

--------------------------------------------------------------------------------
-- **

merge :: DeclarativeItem -> DeclarativePart -> DeclarativePart
merge item []     = [item]
merge item (i:is)
  | item ~=~ i = i `with` item : is
  | otherwise  = i             : merge item is
  where
    (~=~) :: DeclarativeItem -> DeclarativeItem -> Bool
    (~=~) l r = mapIs (const []) l == mapIs (const []) r

    with  :: DeclarativeItem -> DeclarativeItem -> DeclarativeItem
    with  l r = mapIs (++ (getIs r)) l -- this is a bit slow

mergeArchitecture :: DeclarativeItem -> VHDL ()
mergeArchitecture item = modify $ \s -> s { architecture_declarative = item `merge` architecture_declarative s }

mergeProcess      :: DeclarativeItem -> Label -> VHDL ()
mergeProcess item label = modify $ \s -> s { architecture_processes = map go (architecture_processes s) }
  where
    go :: ProcessState -> ProcessState -- this could go wrong, if label isn't unique
    go p | label == process_label p = p { process_declarative = item `merge` process_declarative p }
         | otherwise                = p

--------------------------------------------------------------------------------
-- **

enterGlobal  :: VHDL ()
enterGlobal  = modify $ \s -> s { architecture_process = Global }

enterProcess :: Label -> VHDL ()
enterProcess l = modify $ \s -> s { architecture_process = Labeled l }

newProcess   :: Label -> [Identifier] -> VHDL ()
newProcess l s = modify $ \s -> s { architecture_processes = new : architecture_processes s }
  where
    new :: ProcessState
    new = Process l False (SensitivityList $ map NSimple s) [] Seq.empty

addLocal :: DeclarativeItem -> VHDL ()
addLocal item =
  do state <- get
     case architecture_process state of
       Global    -> mergeArchitecture item
       Labeled l -> mergeProcess      item l

--------------------------------------------------------------------------------
-- **

constantL :: Identifier -> Type -> Maybe Expression -> VHDL Identifier
constantL i t e = addLocal (iConstant [i] t e) >> return i

signalL   :: Identifier -> Type -> Maybe Expression -> VHDL Identifier
signalL   i t e = addLocal (iSignal [i] t Nothing e) >> return i

variableL :: Identifier -> Type -> Maybe Expression -> VHDL Identifier
variableL i t e = addLocal (iVariable False [i] t e) >> return i

fileL     :: Identifier -> Type -> Maybe Expression -> VHDL Identifier
fileL     i t e = addLocal (iFile [i] t file) >> return i
  where file = case e of
          Nothing  -> Nothing
          Just exp -> Just (fileInfo Nothing exp)

--------------------------------------------------------------------------------
-- * Statements
--------------------------------------------------------------------------------

addConcurrentStatement :: ConcurrentStatement -> VHDL ()
addConcurrentStatement s =
  modify $ \state -> state {architecture_statements = architecture_statements state |> s}

addSequentialStatement :: SequentialStatement -> VHDL ()
addSequentialStatement s =
  do current <- gets architecture_process
     case current of
       Global    -> error "can't add sequential statements to main architecture body" -- Hmmm...
       Labeled l -> modify $ \state -> state {architecture_processes = update l (architecture_processes state)}
  where
    update :: Label -> [ProcessState] -> [ProcessState]
    update l [] = []
    update l (p:ps)
      | l == process_label p = p { process_statements = process_statements p |> s } : ps
      | otherwise            = p : update l ps

-- ...

--------------------------------------------------------------------------------
-- **

conSignalAssignment :: Identifier -> Expression -> VHDL ()
conSignalAssignment i e = addConcurrentStatement $ 
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

seqSignalAssignment :: Identifier -> Expression -> VHDL ()
seqSignalAssignment i e = addSequentialStatement $
  SSignalAss
    (SignalAssignmentStatement
      (Nothing)
      (TargetName (NSimple i))
      (Nothing)
      (WaveElem [WaveEExp e Nothing]))

seqVariableAssignment :: Identifier -> Expression -> VHDL ()
seqVariableAssignment i e = addSequentialStatement $
  SVarAss
    (VariableAssignmentStatement
      (Nothing)
      (TargetName (NSimple i))
      (e))

--------------------------------------------------------------------------------
-- * ...
--------------------------------------------------------------------------------

-- | 'a' supports declaration of types
class DeclarativeType a
  where
    declareType    :: TypeDeclaration -> a
    declareSubtype :: SubtypeDeclaration -> a

--------------------------------------------------------------------------------

-- | 'a' supports declaration of constants
class DeclarativeConstant a
  where
    declareConstant :: ConstantDeclaration -> a

-- | 'a' supports declaration of signals
class DeclarativeSignal a
  where
    declareSignal :: SignalDeclaration -> a

-- | 'a' supports declaration of variables
class DeclarativeVariable a
  where
    declareVariable :: VariableDeclaration -> a

-- | 'a' supports declaration of files
class DeclarativeFile a
  where
    declareFile :: FileDeclaration -> a


--------------------------------------------------------------------------------
-- **






--------------------------------------------------------------------------------
-- *
--------------------------------------------------------------------------------

{-
addLocal  ::  Identifier -> Kind -> Type -> Maybe Expression -> VHDL Identifier
addLocal ident kind typ exp =
  do state <- get
     let local  = newLocalDeclaration ident kind typ exp
         locals = addLocalDeclaration local $ architecture_declarative state
     put    $ state { architecture_declarative = locals }
     return $ ident
-}

{-
constantL :: Identifier -> Type -> Maybe Expression -> VHDL Identifier
constantL i typ exp = addLocal   i Constant         typ exp

signalL :: Identifier -> Mode -> Type -> Maybe Expression -> VHDL Identifier
signalL i mod typ exp = addLocal   i Signal            typ exp

variableL :: Identifier -> Mode -> Type -> Maybe Expression -> VHDL Identifier
variableL i mod typ exp = addLocal   i Variable            typ exp

fileL :: Identifier -> Type -> VHDL Identifier
fileL i typ = addLocal   i File         typ Nothing

constantP :: Identifier -> Type -> Maybe Expression -> VHDL Identifier
constantP i typ exp = addProcessLocal i Constant typ exp

variableP :: Identifier -> Mode -> Type -> Maybe Expression -> VHDL Identifier
variableP i mod typ exp = addProcessLocal i Variable typ exp

fileP :: Identifier -> Type -> VHDL Identifier
fileP i typ = addProcessLocal i File typ Nothing
-}
--------------------------------------------------------------------------------
{-
instance DeclarativeType BlockDeclarativeItem
  where
    declareType     = BDIType
    declareSubtype  = BDISubtype

instance DeclarativeKind BlockDeclarativeItem
  where
    declareConstant = BDIConstant
    declareSignal   = BDISignal
    declareVariable = BDIShared
    declareFile     = BDIFile

-}
--------------------------------------------------------------------------------
{-
instance DeclarativeType ProcessDeclarativeItem
  where
    declareType    = PDIType
    declareSubtype = PDISubtype

instance DeclarativeKind ProcessDeclarativeItem
  where
    declareConstant = PDIConstant
    declareSignal   = error "signal declarations are not supported in processes"
    declareVariable = PDIVariable
    declareFile     = PDIFile
-}
--------------------------------------------------------------------------------
