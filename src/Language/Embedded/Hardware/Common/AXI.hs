{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE GADTs               #-}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}

module Language.Embedded.Hardware.Common.AXI (axi_light_signature) where

import Language.Embedded.Hardware hiding (Constraint)
import Language.Embedded.Hardware.Expression.Represent

import Control.Monad.Identity (Identity)
import Control.Monad.Operational.Higher hiding (when)
import Data.Constraint (Constraint)
import Data.Typeable
import Data.Int
import Data.Word
import Data.Bits ()
import Data.Ix (Ix)

import GHC.TypeLits
import qualified GHC.Exts as GHC (Constraint)

import Prelude hiding (not, and, or, div, null)

--------------------------------------------------------------------------------
-- * AXI-light Controller.
--------------------------------------------------------------------------------
-- todo : we make a slight simplification and assume that components which we
--        connect to AXI-lite has a signature of
--          "input -> input -> .. -> input -> output -> ()"
--        this can easily be fixed by inspecting the modes given by the
--        signature.
--------------------------------------------------------------------------------

-- | Short-hand for programs.
type Prog instr exp pred = Program instr (Param2 exp pred)

-- | Short-hand for constraints.
type Pred instr exp pred = (
       SignalCMD      :<: instr
     , ArrayCMD       :<: instr
     , VariableCMD    :<: instr
     , ConditionalCMD :<: instr
     , StructuralCMD  :<: instr
     , LoopCMD        :<: instr
     , ComponentCMD   :<: instr
     , VHDLCMD        :<: instr
     , Hardware exp
     , FreeExp exp
     , PredicateExp exp ~ pred
     , pred (Bit),     PredicateExp exp (Bit)
     , pred (Bits 2),  PredicateExp exp (Bits 2)
     , pred (Bits 3),  PredicateExp exp (Bits 3)
     , pred (Bits 4),  PredicateExp exp (Bits 4)
     , pred (Bits 32), PredicateExp exp (Bits 32)
     , pred (Integer), PredicateExp exp (Integer)
     , Num (exp Integer)
     )

--------------------------------------------------------------------------------
-- ** Signature.

axi_light_signature
  :: forall instr exp pred sig .
     Pred instr exp pred
  => Comp instr exp pred Identity sig
  -> Sig  instr exp pred Identity (
          Signal Bit       -- ^ Global clock signal.
       -> Signal Bit       -- ^ Global reset signal.
       -> Signal (Bits 32) -- ^ Write address.
       -> Signal (Bits 3)  -- ^ Write channel protection type.
       -> Signal Bit       -- ^ Write address valid.
       -> Signal Bit       -- ^ Write address ready.
       -> Signal (Bits 32) -- ^ Write data.
       -> Signal (Bits 4)  -- ^ Write strobes.
       -> Signal Bit       -- ^ Write valid.
       -> Signal Bit       -- ^ Write ready.
       -> Signal (Bits 2)  -- ^ Write response.
       -> Signal Bit       -- ^ Write response valid.
       -> Signal Bit       -- ^ Response ready.
       -> Signal (Bits 32) -- ^ Read address.
       -> Signal (Bits 3)  -- ^ Protection type.
       -> Signal Bit       -- ^ Read address valid.
       -> Signal Bit       -- ^ Read address ready.
       -> Signal (Bits 32) -- ^ Read data.
       -> Signal (Bits 2)  -- ^ Read response.
       -> Signal Bit       -- ^ Read valid.
       -> Signal Bit       -- ^ Read ready.    
       -> ()
     )
axi_light_signature comp =
  exactInput  "S_AXI_ACLK"    $ \s_axi_aclk    ->       
  exactInput  "S_AXI_ARESETN" $ \s_axi_aresetn -> 
  exactInput  "S_AXI_AWADDR"  $ \s_axi_awaddr  ->
  exactInput  "S_AXI_AWPROT"  $ \s_axi_awprot  ->
  exactInput  "S_AXI_AWVALID" $ \s_axi_awvalid -> 
  exactOutput "S_AXI_AWREADY" $ \s_axi_awready ->
  exactInput  "S_AXI_WDATA"   $ \s_axi_wdata   ->
  exactInput  "S_AXI_WSTRB"   $ \s_axi_wstrb   ->
  exactInput  "S_AXI_WVALID"  $ \s_axi_wvalid  ->   
  exactOutput "S_AXI_WREADY"  $ \s_axi_wready  ->   
  exactOutput "S_AXI_BRESP"   $ \s_axi_bresp   ->     
  exactOutput "S_AXI_BVALID"  $ \s_axi_bvalid  ->   
  exactInput  "S_AXI_BREADY"  $ \s_axi_bready  ->   
  exactInput  "S_AXI_ARADDR"  $ \s_axi_araddr  ->
  exactInput  "S_AXI_ARPROT"  $ \s_axi_arprot  ->
  exactInput  "S_AXI_ARVALID" $ \s_axi_arvalid ->   
  exactOutput "S_AXI_ARREADY" $ \s_axi_arready ->
  exactOutput "S_AXI_RDATA"   $ \s_axi_rdata   ->     
  exactOutput "S_AXI_RRESP"   $ \s_axi_rresp   ->     
  exactOutput "S_AXI_RVALID"  $ \s_axi_rvalid  ->
  exactInput  "S_AXI_RREADY"  $ \s_axi_rready  ->   
  ret $ axi_light comp
    s_axi_aclk s_axi_aresetn
    s_axi_awaddr s_axi_awprot s_axi_awvalid s_axi_awready
    s_axi_wdata  s_axi_wstrb  s_axi_wvalid  s_axi_wready
    s_axi_bresp  s_axi_bvalid s_axi_bready
    s_axi_araddr s_axi_arprot s_axi_arvalid s_axi_arready s_axi_rdata
    s_axi_rresp  s_axi_rvalid s_axi_rready     

--------------------------------------------------------------------------------
-- ** Implementation.

axi_light
  :: forall instr exp pred sig .
     Pred instr exp pred
  => Comp instr exp pred Identity sig
  -- AXI.
  -> Signal Bit       -- ^ Global clock signal.
  -> Signal Bit       -- ^ Global reset signal.
  -> Signal (Bits 32) -- ^ Write address.
  -> Signal (Bits 3)  -- ^ Write channel protection type.
  -> Signal Bit       -- ^ Write address valid.
  -> Signal Bit       -- ^ Write address ready.
  -> Signal (Bits 32) -- ^ Write data.
  -> Signal (Bits 4)  -- ^ Write strobes.
  -> Signal Bit       -- ^ Write valid.
  -> Signal Bit       -- ^ Write ready.
  -> Signal (Bits 2)  -- ^ Write response.
  -> Signal Bit       -- ^ Write response valid.
  -> Signal Bit       -- ^ Response ready.
  -> Signal (Bits 32) -- ^ Read address.
  -> Signal (Bits 3)  -- ^ Protection type.
  -> Signal Bit       -- ^ Read address valid.
  -> Signal Bit       -- ^ Read address ready.
  -> Signal (Bits 32) -- ^ Read data.
  -> Signal (Bits 2)  -- ^ Read response.
  -> Signal Bit       -- ^ Read valid.
  -> Signal Bit       -- ^ Read ready.    
  -> Prog instr exp pred ()
axi_light comp
    s_axi_aclk   s_axi_aresetn
    s_axi_awaddr s_axi_awprot s_axi_awvalid s_axi_awready
    s_axi_wdata  s_axi_wstrb  s_axi_wvalid  s_axi_wready
    s_axi_bresp  s_axi_bvalid s_axi_bready
    s_axi_araddr s_axi_arprot s_axi_arvalid s_axi_arready s_axi_rdata
    s_axi_rresp  s_axi_rvalid s_axi_rready
  = do
       ----------------------------------------
       -- AXI Light signals.
       --
       awaddr  <- signal "axi_awaddr"  :: Prog instr exp pred (Signal (Bits 32))
       awready <- signal "axi_awready" :: Prog instr exp pred (Signal (Bit))
       wready  <- signal "axi_wready"  :: Prog instr exp pred (Signal (Bit))
       bresp   <- signal "axi_bresp"   :: Prog instr exp pred (Signal (Bits 2))
       bvalid  <- signal "axi_bvalid"  :: Prog instr exp pred (Signal (Bit))
       araddr  <- signal "axi_araddr"  :: Prog instr exp pred (Signal (Bits 32))
       arready <- signal "axi_arready" :: Prog instr exp pred (Signal (Bit))
       rdata   <- signal "axi_rdata"   :: Prog instr exp pred (Signal (Bits 32))
       rresp   <- signal "axi_rresp"   :: Prog instr exp pred (Signal (Bits 2))
       rvalid  <- signal "axi_rvalid"  :: Prog instr exp pred (Signal (Bit))

       ----------------------------------------
       -- Signals for user logic registers.
       --
       reg_rden  <- signal "slv_reg_rden" :: Prog instr exp pred (Signal (Bit))
       reg_wren  <- signal "slv_reg_wren" :: Prog instr exp pred (Signal (Bit))
       reg_out   <- signal "reg_data_out" :: Prog instr exp pred (Signal (Bits 32))
       reg_index <- signal "byte_index"   :: Prog instr exp pred (Signal (Integer))
       registers <- declareRegisters (signatureOf comp)

       ----------------------------------------
       -- Short-hands for ...
       --
       -- > reset all input registers.
       let mReset  = resetInputs  (signatureOf comp) registers
       -- > reload all input registers.
       let mReload = reloadInputs (signatureOf comp) registers
       -- > fetch the names of all input registers.
       let mInputs = identInputs  (signatureOf comp) registers
       -- > write to output.
       let mWrite :: [When Integer (Prog instr exp pred)]
           mWrite = loadOutputs reg_out 0 (signatureOf comp) registers
       -- > read from input.
       let mRead :: Variable (Bits 32) -> [When Integer (Prog instr exp pred)]
           mRead var = loadInputs s_axi_wdata s_axi_wstrb var 0 (signatureOf comp) (registers)
       
       ----------------------------------------
       -- I/O Connections.
       --
       s_axi_awready <=- awready
       s_axi_wready  <=- wready
       s_axi_bresp   <=- bresp
       s_axi_bvalid  <=- bvalid
       s_axi_arready <=- arready
       s_axi_rdata   <=- rdata
       s_axi_rresp   <=- rresp
       s_axi_rvalid  <=- rvalid

       ----------------------------------------
       -- Mem. mapped register select and write
       -- logic generation.
       --
       u_wr  <- unsafeFreezeSignal wready
       u_wv  <- unsafeFreezeSignal s_axi_wvalid
       u_awr <- unsafeFreezeSignal awready
       u_awv <- unsafeFreezeSignal s_axi_awvalid
       concurrentSetSignal reg_wren
         (u_wr `and` u_wv `and` u_awr `and` u_awv)

       ----------------------------------------
       -- Mem. mapped register select and read
       -- logic generation.
       --
       u_arr <- unsafeFreezeSignal arready
       u_arv <- unsafeFreezeSignal s_axi_arvalid
       u_rv  <- unsafeFreezeSignal rvalid
       concurrentSetSignal reg_rden
         (u_arr `and` u_arv `and` not u_rv)

       ----------------------------------------
       -- AXI_AWREADY generation.
       --
       process (s_axi_aclk .: []) (do
         whenRising s_axi_aclk s_axi_aresetn
           (do awready <== low)
           (do rdy <- getSignal awready
               awv <- getSignal s_axi_awvalid
               wv  <- getSignal s_axi_wvalid
               iff (isLow rdy `and` isHigh awv `and` isHigh wv)
                 (do awready <== high)
                 (do awready <== low)))
           
       ----------------------------------------
       -- AXI_AWADDR latching.
       --
       process (s_axi_aclk .: []) (do
         whenRising s_axi_aclk s_axi_aresetn
           (do awaddr <== zeroes)
           (do rdy <- getSignal awready
               awv <- getSignal s_axi_awvalid
               wv  <- getSignal s_axi_wvalid
               when (isLow  rdy `and` isHigh awv `and` isHigh wv)
                 (awaddr <=- s_axi_awaddr)))

       ----------------------------------------
       -- AXI_WREADY generation.
       --
       process (s_axi_aclk .: []) (do
         whenRising s_axi_aclk s_axi_aresetn
           (do wready <== low)
           (do rdy <- getSignal awready
               awv <- getSignal s_axi_awvalid
               wv  <- getSignal s_axi_wvalid
               iff (isLow  rdy `and` isHigh awv `and` isHigh wv)
                 (wready <== high)
                 (wready <== low)))

       ----------------------------------------
       -- Slave register logic.
       --
       process (s_axi_aclk .: []) (do
         whenRising s_axi_aclk s_axi_aresetn
           (do mReset)
           (do loc_addr <- getBits awaddr (value addr_lsb) (value addr_bits)
               rwren    <- getSignal reg_wren
               temp     <- newVariable
               when (isHigh rwren) $ switched loc_addr
                 (mRead temp)
                 (mReload)))

       ----------------------------------------
       -- Write response logic.
       --
       process (s_axi_aclk .: []) (do
         whenRising s_axi_aclk s_axi_aresetn
           (do bvalid <== low
               bresp  <== zeroes)
           (do awr <- getSignal awready
               awv <- getSignal s_axi_awvalid
               wr  <- getSignal wready
               wv  <- getSignal s_axi_wvalid
               bv  <- getSignal bvalid
               br  <- getSignal s_axi_bready
               ifE ((isHigh awr `and` isHigh awv
                                `and` isHigh wr
                                `and` isHigh wv
                                `and` isLow bv),
                     do bvalid <== high
                        bresp  <== zeroes)
                   ((isHigh br  `and` isHigh bv),
                     do bvalid <== low)))

       ----------------------------------------
       -- AXI_AWREADY generation.
       --
       process (s_axi_aclk .: []) (do
         whenRising s_axi_aclk s_axi_aresetn
           (do arready <== low
               araddr  <== ones)
           (do arr <- getSignal arready
               arv <- getSignal s_axi_arvalid
               iff (isLow arr `and` isHigh arv)
                 (do arready <== high
                     araddr  <=- s_axi_araddr)
                 (do arready <== low)))

       ----------------------------------------
       -- AXI_ARVALID generation.
       --
       process (s_axi_aclk .: []) (do
         whenRising s_axi_aclk s_axi_aresetn
           (do rvalid <== low
               rresp  <== zeroes)
           (do arr <- getSignal arready
               arv <- getSignal s_axi_arvalid
               rv  <- getSignal rvalid
               rr  <- getSignal s_axi_rready
               ifE ((isHigh arr `and` isHigh arv),
                     do rvalid <== high
                        rresp  <== zeroes)
                   ((isHigh rv  `and` isHigh rr),
                     do rvalid <== low)))

       ----------------------------------------
       -- Memory mapped rigister select and
       -- read logic generaiton.
       --
       process (araddr .: s_axi_aresetn .: reg_rden .: mInputs) (do
         loc_addr <- getBits araddr (value addr_lsb) (value addr_bits)
         switched loc_addr
           (mWrite)
           (mReload))

       ----------------------------------------
       -- Output register of memory read data.
       --
       process (s_axi_aclk .: []) (do
         whenRising s_axi_aclk s_axi_aresetn
           (do rdata <== zeroes)
           (do rden <- getSignal reg_rden
               when (isHigh rden)
                 (do rdata <=- reg_out)))

       ----------------------------------------
       -- User logic.
       --
       portmap comp registers
       --
       -- The end.
       ----------------------------------------
  where
    -- Application-specific design signals.
    addr_lsb, addr_bits :: Integer
    addr_lsb  = 2
    addr_bits = addr_lsb + (widthOf comp)

--------------------------------------------------------------------------------
-- ** Helpers.

-- | Declare the registers which will be used by our AXI-lite slave to store
--   values received from the master and, once filled, as input for the comp.
declareRegisters :: forall instr (exp :: * -> *) pred m a .
     Pred instr exp pred
  => Sig  instr exp pred Identity a
  -> Prog instr exp pred (Argument pred a)
declareRegisters (Ret _) = return Nil
declareRegisters (SSig _ _ sf) =
  do s <- newSignal
     a <- declareRegisters (sf s)
     return (ASig s a)
declareRegisters (SArr _ _ l af) =
  do s <- newArray (litE l)
     a <- declareRegisters (af s)
     return (AArr s a)

--------------------------------------------------------------------------------

-- | ...
resetInputs :: forall instr (exp :: * -> *) pred m a .
     Pred instr exp pred
  => Sig  instr exp pred Identity a
  -> Argument pred a
  -> Prog instr exp pred ()
resetInputs (Ret _) (Nil) =
  do return ()
resetInputs (SSig _ In sf) (ASig s arg) =
  do setSignal s (litE reset)
     resetInputs (sf s) arg
resetInputs (SSig _ _ sf) (ASig s arg) =
  do resetInputs (sf s) arg
resetInputs (SArr _ In _ af) (AArr a arg) =
  do resetArray a (litE reset)
     resetInputs (af a) arg
resetInputs (SArr _ _ _ af) (AArr a arg) =
  do resetInputs (af a) arg

--------------------------------------------------------------------------------

-- | ...
reloadInputs :: forall instr (exp :: * -> *) pred m a .
     Pred instr exp pred
  => Sig  instr exp pred Identity a
  -> Argument pred a
  -> Prog instr exp pred ()
reloadInputs (Ret _) (Nil) =
  do return ()
reloadInputs (SSig _ In sf) (ASig s arg) =
  do sv <- unsafeFreezeSignal s
     setSignal s sv
     reloadInputs (sf s) arg
reloadInputs (SSig _ _ sf) (ASig s arg) =
  do reloadInputs (sf s) arg
reloadInputs (SArr _ In l af) (AArr a arg) =
  do copyArray (a, litE 0) (a, litE 0) (litE l)
     reloadInputs (af a) arg
reloadInputs (SArr _ _ _ af) (AArr a arg) =
  do reloadInputs (af a) arg

--------------------------------------------------------------------------------

-- | ...
loadInputs :: forall instr (exp :: * -> *) pred m a .
     Pred instr exp pred
  => Signal   (Bits 32) -- ^ Input.
  -> Signal   (Bits 4)  -- ^ Protected bits.
  -> Variable (Bits 32) -- ^ Temp.
  -> Integer            -- ^ Index.
  -> Sig  instr exp pred Identity a
  -> Argument pred a
  -> [When Integer (Prog instr exp pred)]
loadInputs wdata wren tmp i (Ret _) (Nil) = []
loadInputs wdata wren tmp i (SSig _ Out sf) (ASig s arg) =
    loadInputs wdata wren tmp (i+1) (sf s) arg
loadInputs wdata wren tmp i (SArr _ Out l af) (AArr a arg) =
    loadInputs wdata wren tmp (i+(Prelude.toInteger l)) (af a) arg
loadInputs wdata wren tmp i (SSig _ In sf) (ASig s arg) =
    When (Is i) cases : loadInputs wdata wren tmp (i+1) (sf s) arg
  where
    size :: Integer
    size = bits s

    loadBit :: Prog instr exp pred ()
    loadBit = do
      wb <- getBit wren 0
      when (isHigh wb) $
        do bit <- getBit wdata 0
           setBit s 0 bit

    loadBits :: Integer -> Integer -> Prog instr exp pred ()
    loadBits ix len = do
      wb <- getBit wren (value ix)
      when (isHigh wb) $
        copyBits (s, value $ ix*8) (wdata, value $ ix*8) (value $ len-1)

    cases :: Prog instr exp pred ()
    cases | size == 1 = loadBit
          | otherwise = sequence_ $ map (uncurry loadBits) $ zip [0..] $ chunk size
{-
loadInputs wdata wren tmp i (SArr _ In l af) (AArr a arg) =
    let cs = map (\ix -> When (Is $ i+ix) $ cases ix) [0..l-1]
     in cs ++ loadInputs wdata wren tmp (i+l) (af a) arg
  where
    size :: Integer
    size = bits a

    loadBit :: Integer -> Prog instr exp pred ()
    loadBit ax = error "axi-todo: loadBit for array."

    loadBits :: Integer -> Integer -> Integer -> Prog instr exp pred ()
    loadBits ax ix len = do
      wb <- getBit wren (value ix)
      when (isHigh wb) $
        copyVBits (tmp, value $ ix*8) (wdata, value $ ix*8) (value $ len-1)

    cases :: Integer -> Prog instr exp pred ()
    cases ax | size == 1 = loadBit ax
             | otherwise = do
      sequence_ $ map (uncurry $ loadBits ax) $ zip [0..] $ chunk size
      val <- unsafeFreezeVariable tmp
      setArray a (value ax) (fromBits val)
-}

-- | ...
loadOutputs :: forall instr (exp :: * -> *) pred m a .
     (Pred instr exp pred, Monad m)
  => Signal (Bits 32) -- ^ Output.
  -> Integer          -- ^ Index.
  -> Sig instr exp pred m a
  -> Argument pred a
  -> [When Integer (ProgramT instr (Param2 exp pred) m)]
loadOutputs o i (Ret _) (Nil) = []
{-
loadOutputs o i (SSig _ Out sf) (ASig s arg) =
  let p = setSignal o . toBits =<< unsafeFreezeSignal s
   in When (Is i) p : loadOutputs o (i+1) (sf s) arg
loadOutputs o i (SSig _ _ sf) (ASig s arg) =
  loadOutputs o (i+1) (sf s) arg
loadOutputs o i (SArr _ Out l af) (AArr a arg) =
  let f ix = When (Is ix) (setSignal o . toBits =<< getArray a (value ix))
   in map f [i..i+l-1] ++ loadOutputs o (i+l) (af a) arg
loadOutputs o i (SArr _ _ l af) (AArr a arg) =
  loadOutputs o (i+l) (af a) arg
-}
chunk :: Integer -> [Integer]
chunk i | i >  8 = 8 : chunk (i - 8)
        | i <= 8 = [i]

--------------------------------------------------------------------------------

identInputs :: forall instr (exp :: * -> *) pred m a .
     Sig instr exp pred m a
  -> Argument pred a
  -> [Ident]
identInputs (Ret _) (Nil) = []
identInputs (SSig _ In sf)   (ASig s arg) = toIdent s : identInputs (sf s) arg
identInputs (SSig _ _  sf)   (ASig s arg) = identInputs (sf s) arg
identInputs (SArr _ In _ af) (AArr a arg) = toIdent a : identInputs (af a) arg
identInputs (SArr _ _  _ af) (AArr a arg) = identInputs (af a) arg

--------------------------------------------------------------------------------

signatureOf :: Comp instr exp pred m a -> Sig instr exp pred m a
signatureOf (Component _ sig) = sig

widthOf :: Comp instr exp pred m a -> Integer
widthOf = go . signatureOf
  where
    go :: Sig instr exp pred m b -> Integer
    go (Ret _)        = 0
    go (SSig _ _ f)   = 1 + go (f dummy)
  --go (SArr _ _ l g) = l + go (g dummy)

dummy :: a
dummy = error "todo: evaluated dummy"

--------------------------------------------------------------------------------

high, low :: Expr exp => exp Bit
high = true
low  = false

isHigh, isLow :: (Expr exp, Rel exp) => exp Bit -> exp Bit
isHigh e = e `eq` high
isLow  e = e `eq` low

--------------------------------------------------------------------------------

zeroes :: (Primary exp, Typeable n, KnownNat n) => exp (Bits n)
zeroes = value 0

ones :: forall exp n. (Primary exp, Typeable n, KnownNat n) => exp (Bits n)
ones = value $ bitFromInteger (read (replicate size '1') :: Integer)
  where size = fromIntegral (ni (Proxy::Proxy n)) - 1

--------------------------------------------------------------------------------
