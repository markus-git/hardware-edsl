module Language.Embedded.VHDL
  ( Mode(..), Type(..), Kind(..)
  , std_logic
  , signed,  signed8,  signed16,  signed32,  signed64
  , usigned, usigned8, usigned16, usigned32, usigned64
                                             
  , module Language.Embedded.VHDL.Interface
  , module Language.Embedded.VHDL.Expression
  , module Language.Embedded.VHDL.Command
  ) where

import Language.VHDL (Mode(..))
import Language.Embedded.VHDL.Interface
import Language.Embedded.VHDL.Expression
import Language.Embedded.VHDL.Command
import Language.Embedded.VHDL.Monad.Type
