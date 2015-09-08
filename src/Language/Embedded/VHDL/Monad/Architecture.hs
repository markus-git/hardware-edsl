module Language.Embedded.VHDL.Monad.Architecture where

import Language.VHDL

--------------------------------------------------------------------------------
-- * Architecture manipulation
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- **

class DeclarativeType a
  where
    declareType                       :: TypeDeclaration            -> a
    declareSubtype                    :: SubtypeDeclaration         -> a

class DeclarativeKind a
  where
    declareConstant                   :: ConstantDeclaration        -> a
    declareSignal                     :: SignalDeclaration          -> a
    declareVariable                   :: VariableDeclaration        -> a
    declareFile                       :: FileDeclaration            -> a

--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------

instance DeclarativeType ProcessDeclarativeItem
  where
    declareType    = PDIType
    declareSubtype = PDISubtype

--------------------------------------------------------------------------------
    
{-
newLocalDeclaration :: Identifier -> Kind -> Type -> Maybe Expression -> BlockDeclarativeItem
newLocalDeclaration ident kind typ exp = case kind of
  Constant -> BDIConstantDecl $ ConstantDeclaration       [ident] typ         exp
  Signal   -> BDISignalDecl   $ SignalDeclaration         [ident] typ Nothing exp
  Variable -> BDISharedDecl   $ VariableDeclaration False [ident] typ         exp
  File     -> BDIFileDecl     $ FileDeclaration           [ident] typ file
    where file = case exp of
            Nothing -> Nothing
            Just e  -> Just $ FileOpenInformation Nothing e

newProcDeclaration :: Identifier -> Kind -> Type -> Maybe Expression -> ProcessDeclarativeItem
newProcDeclaration ident kind typ exp = case kind of
  Constant -> ProcDIConstant $ ConstantDeclaration       [ident] typ         exp
  Variable -> ProcDIVariable $ VariableDeclaration False [ident] typ         exp
  File     -> ProcDIFile     $ FileDeclaration           [ident] typ file
    where file = case exp of
            Nothing -> Nothing
            Just e  -> Just $ FileOpenInformation Nothing e
-}

{-
addLocalDeclaration :: BlockDeclarativeItem -> ArchitectureDeclarativePart -> ArchitectureDeclarativePart
addLocalDeclaration new blocks = insert new blocks
  where
    insert :: BlockDeclarativeItem -> [BlockDeclarativeItem] -> [BlockDeclarativeItem]
    insert new [] = [new]
    insert new (b:bs)
      | b ~=~ new = b `add` new : bs
      | otherwise = b           : insert new bs

    (~=~)  :: BlockDeclarativeItem -> BlockDeclarativeItem -> Bool
    (~=~) l r = mappy (const []) l == mappy (const []) r

    add    :: BlockDeclarativeItem -> BlockDeclarativeItem -> BlockDeclarativeItem
    add l r = mappy (getty r :) l

    mappy :: (IdentifierList -> IdentifierList) -> BlockDeclarativeItem -> BlockDeclarativeItem
    mappy f b = case b of
      (BDIConstantDecl l) -> BDIConstantDecl $ l {const_identifier_list  = f (const_identifier_list l)}
      (BDISignalDecl l)   -> BDISignalDecl   $ l {signal_identifier_list = f (signal_identifier_list l)}
      (BDISharedDecl l)   -> BDISharedDecl   $ l {var_identifier_list    = f (var_identifier_list l)}
      (BDIFileDecl l)     -> BDIFileDecl     $ l {fd_identifier_list     = f (fd_identifier_list l)}

    getty :: BlockDeclarativeItem -> Identifier
    getty b = case b of
      (BDIConstantDecl l) -> head $ const_identifier_list l
      (BDISignalDecl l)   -> head $ signal_identifier_list l
      (BDISharedDecl l)   -> head $ var_identifier_list l
      (BDIFileDecl l)     -> head $ fd_identifier_list l


addProcDeclaration :: ProcessDeclarativeItem -> ProcessDeclarativePart -> ProcessDeclarativePart
addProcDeclaration new blocks = insert new blocks
  where
    insert :: ProcessDeclarativeItem -> [ProcessDeclarativeItem] -> [ProcessDeclarativeItem]
    insert new [] = [new]
    insert new (b:bs)
      | b ~=~ new = b `add` new : bs
      | otherwise = b           : insert new bs

    (~=~)  :: ProcessDeclarativeItem -> ProcessDeclarativeItem -> Bool
    (~=~) l r = mappy (const []) l == mappy (const []) r

    add    :: ProcessDeclarativeItem -> ProcessDeclarativeItem -> ProcessDeclarativeItem
    add l r = mappy (getty r :) l

    mappy :: (IdentifierList -> IdentifierList) -> ProcessDeclarativeItem -> ProcessDeclarativeItem
    mappy f b = case b of
      (ProcDIConstant l) -> ProcDIConstant $ l {const_identifier_list  = f (const_identifier_list l)}
      (ProcDIVariable l) -> ProcDIVariable $ l {var_identifier_list    = f (var_identifier_list l)}
      (ProcDIFile l)     -> ProcDIFile     $ l {fd_identifier_list     = f (fd_identifier_list l)}

    getty :: ProcessDeclarativeItem -> Identifier
    getty b = case b of
      (ProcDIConstant l) -> head $ const_identifier_list l
      (ProcDIVariable l) -> head $ var_identifier_list l
      (ProcDIFile l)     -> head $ fd_identifier_list l
-}

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
