{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Embedded.VHDL.Monad.VHDL where

import Language.VHDL

import Control.Applicative hiding (empty)
import Control.Monad
import Control.Monad.State hiding (lift)

import Data.Foldable (toList)
import Data.Map      (Map)
import Data.Set      (Set)
import Data.Sequence (Seq, (|>))
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
  , entity_generics          :: Maybe GenericClause      -- Set InterfaceElement
  , entity_ports             :: Maybe PortClause         -- Set InterfaceElement
  , entity_declarative       :: Seq EntityDeclarativeItem
  , entity_statements        :: Seq EntityStatement
  }

data ArchitectureState = Architecture {
    architecture_ident       :: String
  , architecture_header      :: EntityState
  , architecture_declarative :: ArchitectureDeclarativePart
  , architecture_statements  :: Seq ConcurrentStatement
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
    , ArchitectureBody  (Ident ai) (NSimple (Ident ei)) (toList ad) (toList  as))
  where
    (Architecture ai (Entity ei eg ep ed es) ad as) = execState (unVHDL m) s

    toList' xs
      | P.null xs = Nothing
      | otherwise = Just $ toList xs

--------------------------------------------------------------------------------

emptyArchitectureState :: String -> String -> ArchitectureState
emptyArchitectureState i n =
  Architecture i
    (Entity n Nothing Nothing Seq.empty Seq.empty) [] Seq.empty

behavioural, structural :: String -> ArchitectureState
behavioural = emptyArchitectureState "behavioural"
structural  = emptyArchitectureState "structural"

--------------------------------------------------------------------------------
