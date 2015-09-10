{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- used for the Ord/Eq inst. of XDeclaration etc.
{-# LANGUAGE StandaloneDeriving #-}

module Language.Embedded.VHDL.Monad (
    Kind(..)
  , Type(..)
  , VHDL
    
    -- ^ run
  , runVHDL
  , behavioural
  , structural

    -- ^ assignment
    -- port  / generic  / global / local
  , constant, constantGeneric, constantG, constantL
  , signal,   signalGeneric,   signalG,   signalL
  , variable, variableGeneric, variableG, variableL
  , file,     fileGeneric,     fileG,     fileL

    -- ^ ..
  , conSignalAssignment

    -- ^ ..
  , seqSignalAssignment
  , seqVariableAssignment
    
  , newProcess
  , enterProcess
  , enterGlobal

    -- ^ expressions
  , and, or, xor, xnor
  , eq, neq, lt, lte, gt, gte
  , nand, nor
  , sll, srl, sla, sra, rol, ror
  , add, sub, cat
  , neg
  , mul, div, mod, rem
  , exp
  , abs, not
         
  , name, string, lit, null

  , resize

    -- ^ types
  , std_logic
  , signed,  signed8,  signed16,  signed32,  signed64
  , usigned, usigned8, usigned16, usigned32, usigned64
  ) where

import Language.VHDL
import Language.Embedded.VHDL.Expression.Hoist hiding (Kind)
import Language.Embedded.VHDL.Monad.VHDL
import Language.Embedded.VHDL.Monad.Type
import Language.Embedded.VHDL.Monad.Expression
import Language.Embedded.VHDL.Monad.Entity
import Language.Embedded.VHDL.Monad.Architecture

import Prelude hiding (null, not, abs, exp, rem, mod, div, and, or)

--------------------------------------------------------------------------------
-- Ord instance for use in sequence
--------------------------------------------------------------------------------

instance Ord BlockDeclarativeItem
  where
    compare (BDIConstant l) (BDIConstant r) = compare l r
    compare (BDISignal l)   (BDISignal r)   = compare l r
    compare (BDIShared l)   (BDIShared r)   = compare l r
    compare (BDIFile l)     (BDIFile r)     = compare l r
    compare l               r               = compare (ordBlock l) (ordBlock r)
      where
        ordBlock :: BlockDeclarativeItem -> Int
        ordBlock block = case block of
          (BDIConstant r) -> 1
          (BDISignal r)   -> 2
          (BDIShared r)   -> 3
          (BDIFile r)     -> 4

instance Ord ConstantDeclaration
  where
    compare l r = compare (const_identifier_list l) (const_identifier_list r)

instance Ord SignalDeclaration
  where
    compare l r = compare (signal_identifier_list l) (signal_identifier_list r)

instance Ord VariableDeclaration
  where
    compare l r = compare (var_shared l) (var_shared r)

instance Ord FileDeclaration
  where
    compare l r = compare (fd_identifier_list l) (fd_identifier_list r)

deriving instance Ord Identifier

--------------------------------------------------------------------------------
