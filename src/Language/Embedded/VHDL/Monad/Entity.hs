module Language.Embedded.VHDL.Monad.Entity
  ( addPort
  , addGeneric

  , constant, constantG -- :: Identifier         -> Type -> Maybe Expression -> VHDL Identifier
  , signal,   signalG   -- :: Identifier -> Mode -> Type -> Maybe Expression -> VHDL Identifier
  , variable, variableG -- :: Identifier -> Mode -> Type -> Maybe Expression -> VHDL Identifier
  , file,     fileG     -- :: Identifier         -> Type                     -> VHDL Identifier
  ) where

import Language.VHDL

import Language.Embedded.VHDL.Monad.VHDL
import Language.Embedded.VHDL.Monad.Type

import Control.Monad
import Control.Monad.State hiding (lift)

--------------------------------------------------------------------------------
-- * Entity manipulation
--------------------------------------------------------------------------------

-- | Creates a new interface declaration from the given attributes
newInterfaceDeclaration :: Identifier -> Kind -> Maybe Mode -> Type -> Maybe Expression -> InterfaceDeclaration
newInterfaceDeclaration ident kind mode typ exp = case kind of
  Constant -> InterfaceConstantDeclaration [ident]      typ       exp
  Signal   -> InterfaceSignalDeclaration   [ident] mode typ False exp
  Variable -> InterfaceVariableDeclaration [ident] mode typ       exp
  File     -> InterfaceFileDeclaration     [ident]      typ

-- | Adds a new interface declaration to a list of existing ones
addInterfaceDeclaration :: InterfaceDeclaration -> InterfaceList -> InterfaceList
addInterfaceDeclaration new (InterfaceList is) = InterfaceList (insert new is)
  where
    insert :: InterfaceElement -> [InterfaceElement] -> [InterfaceElement]
    insert new [] = [new]
    insert new (i:is)
      | i ~=~ new  = i `add` new : is
      | otherwise  = i           : insert new is

    -- equality which ignores identifiers (trying to find the correct group)
    (~=~)  :: InterfaceElement -> InterfaceElement -> Bool
    (~=~) l r = l { idecl_identifier_list = [] } == r { idecl_identifier_list = [] }

    -- adds a new identifier to the old ones
    add    :: InterfaceElement -> InterfaceElement -> InterfaceElement
    add old new = let [n] = idecl_identifier_list new in
      old { idecl_identifier_list = n : idecl_identifier_list old }

--------------------------------------------------------------------------------
-- ** Declaring new entity interfaces

-- | Declares a new port interface
addPort :: Identifier -> Kind -> Maybe Mode -> Type -> Maybe Expression -> VHDL Identifier
addPort ident kind mode typ exp =
  do state <- get
     let port  = newInterfaceDeclaration ident kind mode typ exp
         ports = case entity_ports (architecture_header state) of
           Nothing              -> PortClause (InterfaceList [port])
           Just (PortClause is) -> PortClause (addInterfaceDeclaration port is)
     put    $ state { architecture_header = (architecture_header state) { entity_ports = Just ports } }
     return $ ident

-- | Declares a new generec interface
addGeneric :: Identifier -> Kind -> Maybe Mode -> Type -> Maybe Expression -> VHDL Identifier
addGeneric ident kind mode typ exp =
  do state <- get
     let gen  = newInterfaceDeclaration ident kind mode typ exp
         gens = case entity_generics (architecture_header state) of
           Nothing                 -> GenericClause (InterfaceList [gen])
           Just (GenericClause is) -> GenericClause (addInterfaceDeclaration gen is)
     put    $ state { architecture_header = (architecture_header state) { entity_generics = Just gens } }
     return $ ident

--------------------------------------------------------------------------------
-- ** Common declarations

constant, constantG :: Identifier -> Type -> Maybe Expression -> VHDL Identifier
constant  i typ exp = addPort    i Constant Nothing typ exp
constantG i typ exp = addGeneric i Constant Nothing typ exp

signal, signalG :: Identifier -> Mode -> Type -> Maybe Expression -> VHDL Identifier
signal  i mod typ exp = addPort    i Signal (Just mod) typ exp
signalG i mod typ exp = addGeneric i Signal (Just mod) typ exp

variable, variableG :: Identifier -> Mode -> Type -> Maybe Expression -> VHDL Identifier
variable  i mod typ exp = addPort    i Variable (Just mod) typ exp
variableG i mod typ exp = addGeneric i Variable (Just mod) typ exp

file, fileG :: Identifier -> Type -> VHDL Identifier
file  i typ = addPort    i File Nothing typ Nothing
fileG i typ = addGeneric i File Nothing typ Nothing

--------------------------------------------------------------------------------
