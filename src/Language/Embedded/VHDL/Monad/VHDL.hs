{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Embedded.VHDL.Monad.VHDL where

import Language.VHDL

import Control.Applicative hiding (empty)
import Control.Monad
import Control.Monad.State hiding (lift)

import Data.Foldable (toList)
import Data.Map      (Map)
import Data.Set      (Set)
import Data.Sequence (Seq, (|>), (<|))
import qualified Data.Map      as Map
import qualified Data.Set      as Set
import qualified Data.Sequence as Seq

import Prelude
import qualified Prelude as P

--------------------------------------------------------------------------------
-- * VHDL Generation Monad
--------------------------------------------------------------------------------

data EntityState = Entity {
    entity_ident             :: String
  , entity_generics          :: Maybe GenericClause
  , entity_ports             :: Maybe PortClause
  , entity_declarative       :: Seq EntityDeclarativeItem
  , entity_statements        :: Seq EntityStatement
  }

data GlobalLabel  = Global | Labeled Label

data ProcessState = Process {
    process_label            :: Label
  , process_postponed        :: Bool
  , process_sensitivity_list :: SensitivityList
  , process_declarative      :: DeclarativePart
  , process_statements       :: Seq SequentialStatement
  }

data ArchitectureState = Architecture {
    architecture_ident       :: String
  , architecture_header      :: EntityState
  , architecture_declarative :: DeclarativePart
  , architecture_statements  :: Seq ConcurrentStatement
    
    -- process state lifted to here
  , architecture_process     :: GlobalLabel
  , architecture_processes   :: [ProcessState]
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
    , ArchitectureBody  (Ident ai) (NSimple (Ident ei)) (toADP  ad) (toList  as'))
  where
    (Architecture ai (Entity ei eg ep ed es) ad as _ ps) = execState (unVHDL m) s
    
    as' = foldr (flip (|>)) as $ map toProc ps
    
    toProc :: ProcessState -> ConcurrentStatement
    toProc (Process l p sl d s) =
        ConProcess $ ProcessStatement (Just l) p (sense sl) (toPDP d) (toList s)
      where
        sense :: SensitivityList -> Maybe SensitivityList
        sense (SensitivityList []) = Nothing
        sense sl                   = Just sl

toList' :: Foldable f => f a -> Maybe [a]
toList' xs
  | null xs   = Nothing
  | otherwise = Just $ toList xs

--------------------------------------------------------------------------------

emptyArchitectureState :: String -> String -> ArchitectureState
emptyArchitectureState i n =
  Architecture i
    (Entity n Nothing Nothing Seq.empty Seq.empty) [] Seq.empty
    Global []

behavioural, structural :: String -> ArchitectureState
behavioural = emptyArchitectureState "behavioural"
structural  = emptyArchitectureState "structural"

--------------------------------------------------------------------------------
-- *
--------------------------------------------------------------------------------

-- | ...
data DeclarativeItem =
    C ConstantDeclaration
  | S   SignalDeclaration
  | V VariableDeclaration
  | F     FileDeclaration
  deriving Eq

-- | ...
type DeclarativePart = [DeclarativeItem]

--------------------------------------------------------------------------------
-- ! I should make classes or something for these...

-- | ...
toADP :: DeclarativePart -> ArchitectureDeclarativePart
toADP = map toB
  where
    toB :: DeclarativeItem -> BlockDeclarativeItem
    toB (C c) = BDIConstant c
    toB (S s) = BDISignal s
    toB (V v) = BDIShared v
    toB (F f) = BDIFile f

-- | ...
toPDP :: DeclarativePart -> ProcessDeclarativePart
toPDP = map toP
  where
    toP :: DeclarativeItem -> ProcessDeclarativeItem
    toP (C c) = PDIConstant c
    toP (S s) = error (show s)
    toP (V v) = PDIVariable v
    toP (F f) = PDIFile f

--------------------------------------------------------------------------------
