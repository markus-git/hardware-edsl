module Language.Embedded.Hardware.Expression
  ( HExp
  , HType
  , module Language.Embedded.Hardware.Expression.Frontend
  ) where

import Language.Embedded.Hardware.Expression.Syntax (HExp, HType)
import Language.Embedded.Hardware.Expression.Frontend
import Language.Embedded.Hardware.Expression.Backend.VHDL ()
