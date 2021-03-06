{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE Rank2Types            #-}

module Language.Embedded.Hardware.Interface.AXI (axi_light, AXIPred, FreePrim(..)) where

import Language.Embedded.VHDL (Mode(..))
import Language.Embedded.Hardware.Command.CMD
import Language.Embedded.Hardware.Command.Frontend
import Language.Embedded.Hardware.Interface
import Language.Embedded.Hardware.Expression.Frontend
import Language.Embedded.Hardware.Expression.Represent
import Language.Embedded.Hardware.Expression.Represent.Bit (Bits, Bit, bitFromInteger, ni)

-- hmm...
import Language.Embedded.Hardware.Expression.Syntax (HExp, HType)
import Language.Embedded.Hardware.Expression.Backend.VHDL ()

import Control.Monad.Identity (Identity)
import Control.Monad.Operational.Higher hiding (when)
import Data.Constraint (Constraint)
import Data.Typeable
import Data.Int
import Data.Word
import Data.Bits ()
import Data.Ix (Ix)

import Data.Constraint

import GHC.TypeLits
import qualified GHC.Exts as GHC (Constraint)

import Prelude hiding (not, and, or, div, null)
import qualified Prelude as P

--------------------------------------------------------------------------------
-- * AXI-light Controller.
--------------------------------------------------------------------------------

-- | Make sure that `pred a` implies `PredicateExp exp a`.
class FreeExp exp => FreePrim exp pred
  where
    witPred :: PrimType a => Proxy exp -> Dict (pred a) -> Dict (PredicateExp exp a)

instance FreePrim HExp HType
  where
    witPred _ _ = Dict

litP :: forall exp pred a . (FreePrim exp pred, PrimType a, pred a)
  => Proxy pred -> a -> exp a
litP _ a = case witPred (Proxy :: Proxy exp) (Dict :: Dict (pred a)) of
  Dict -> litE a

--------------------------------------------------------------------------------

-- | Short-hand for programs.
type Prog instr exp pred = Program instr (Param2 exp pred)

-- | Short-hand for constraints.
type AXIPred instr exp pred = (
     -- Instructions.
       SignalCMD      :<: instr
     , ArrayCMD       :<: instr
     , VariableCMD    :<: instr
     , ConditionalCMD :<: instr
     , ProcessCMD     :<: instr
     , LoopCMD        :<: instr
     , ComponentCMD   :<: instr
     , VHDLCMD        :<: instr
     -- Expressions.
     , Expr    exp
     , Rel     exp
     , Factor  exp
     , Primary exp
     -- 
     , FreeExp  exp
     , FreePrim exp pred
     --
     , PredicateExp exp (Bit)
     , PredicateExp exp (Bits 2)
     , PredicateExp exp (Bits 3)
     , PredicateExp exp (Bits 4)
     , PredicateExp exp (Bits 32)
     , PredicateExp exp (Integer)
     --
     , pred (Bit)
     , pred (Bits 2)
     , pred (Bits 3)
     , pred (Bits 4)
     , pred (Bits 32)
     , pred (Integer)
     --
     , Num (exp Integer)
     )

--------------------------------------------------------------------------------
-- ** Signature.
--------------------------------------------------------------------------------

axi_light
  :: forall instr exp pred sig . AXIPred instr exp pred
  => Comp instr exp pred Identity sig
  -> Sig  instr exp pred Identity (
          Signal (Bits 32) -- ^ Write address.
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
axi_light comp =
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
  ret $ axi_light_impl comp
    s_axi_awaddr s_axi_awprot s_axi_awvalid s_axi_awready
    s_axi_wdata  s_axi_wstrb  s_axi_wvalid  s_axi_wready
    s_axi_bresp  s_axi_bvalid s_axi_bready
    s_axi_araddr s_axi_arprot s_axi_arvalid s_axi_arready s_axi_rdata
    s_axi_rresp  s_axi_rvalid s_axi_rready

--------------------------------------------------------------------------------
-- ** Implementation.
--------------------------------------------------------------------------------

axi_light_impl
  :: forall instr exp pred sig . AXIPred instr exp pred
  -- Component to connect:
  => Comp instr exp pred Identity sig
  -- AXI signals:
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
axi_light_impl comp
    s_axi_awaddr s_axi_awprot s_axi_awvalid s_axi_awready
    s_axi_wdata  s_axi_wstrb  s_axi_wvalid  s_axi_wready
    s_axi_bresp  s_axi_bvalid s_axi_bready
    s_axi_araddr s_axi_arprot s_axi_arvalid s_axi_arready s_axi_rdata
    s_axi_rresp  s_axi_rvalid s_axi_rready
  = do
       ----------------------------------------
       -- AXI Light signals.
       --
       awaddr  :: Signal (Bits 32) <- newNamedSignal "axi_awaddr"
       awready :: Signal (Bit)     <- newNamedSignal "axi_awready"
       wready  :: Signal (Bit)     <- newNamedSignal "axi_wready"
       bresp   :: Signal (Bits 2)  <- newNamedSignal "axi_bresp"
       bvalid  :: Signal (Bit)     <- newNamedSignal "axi_bvalid"
       araddr  :: Signal (Bits 32) <- newNamedSignal "axi_araddr"
       arready :: Signal (Bit)     <- newNamedSignal "axi_arready"
       rdata   :: Signal (Bits 32) <- newNamedSignal "axi_rdata"
       rresp   :: Signal (Bits 2)  <- newNamedSignal "axi_rresp"
       rvalid  :: Signal (Bit)     <- newNamedSignal "axi_rvalid"

       ----------------------------------------
       -- Signals for user logic registers.
       --
       reg_rden  :: Signal (Bit)     <- newNamedSignal "slv_reg_rden"
       reg_wren  :: Signal (Bit)     <- newNamedSignal "slv_reg_wren"
       reg_out   :: Signal (Bits 32) <- newNamedSignal "reg_data_out"
       reg_index :: Signal (Integer) <- newNamedSignal "byte_index"
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
       -- > fetch the names of all output registers.
       let mOutputs = identOutputs (signatureOf comp) registers
       -- > write to output.
       let mWrite :: Prog instr exp pred ()
           mWrite = loadOutputs araddr reg_out
             (addr_lsb) (addr_bits)         -- sizes.
             (signatureOf comp) (registers) -- sig and args.
       -- > read from input.
       let mRead :: Prog instr exp pred ()
           mRead = loadInputs awaddr reg_wren s_axi_wdata s_axi_wstrb
             (addr_lsb) (addr_bits)         -- sizes.
             (signatureOf comp) (registers) -- sig and args.
       
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
       setSignal reg_wren
         (u_wr `and` u_wv `and` u_awr `and` u_awv)

       ----------------------------------------
       -- Mem. mapped register select and read
       -- logic generation.
       --
       u_arr <- unsafeFreezeSignal arready
       u_arv <- unsafeFreezeSignal s_axi_arvalid
       u_rv  <- unsafeFreezeSignal rvalid
       setSignal reg_rden
         (u_arr `and` u_arv `and` not u_rv)

       ----------------------------------------
       -- AXI_AWREADY generation.
       --
       -- processR s_axi_aclk s_axi_aresetn []
       processR []
         (do awready <== low)
         (do rdy <- unsafeFreezeSignal awready
             awv <- unsafeFreezeSignal s_axi_awvalid
             wv  <- unsafeFreezeSignal s_axi_wvalid
             iff (isLow rdy `and` isHigh awv `and` isHigh wv)
               (do awready <== high)
               (do awready <== low))
       
       ----------------------------------------
       -- AXI_AWADDR latching.
       --
       processR []
         (do awaddr <== zeroes)
         (do rdy <- unsafeFreezeSignal awready
             awv <- unsafeFreezeSignal s_axi_awvalid
             wv  <- unsafeFreezeSignal s_axi_wvalid
             when (isLow  rdy `and` isHigh awv `and` isHigh wv)
               (do awaddr <=- s_axi_awaddr))

       ----------------------------------------
       -- AXI_WREADY generation.
       --
       processR []
         (do wready <== low)
         (do rdy <- unsafeFreezeSignal awready
             awv <- unsafeFreezeSignal s_axi_awvalid
             wv  <- unsafeFreezeSignal s_axi_wvalid
             iff (isLow  rdy `and` isHigh wv `and` isHigh awv)
               (do wready <== high)
               (do wready <== low))

       ----------------------------------------
       -- Slave register logic.
       --
       processR [] (mReset) (mRead)
       
       ----------------------------------------
       -- Write response logic.
       --
       processR []
         (do bvalid <== low
             bresp  <== zeroes)
         (do awr <- unsafeFreezeSignal awready
             awv <- unsafeFreezeSignal s_axi_awvalid
             wr  <- unsafeFreezeSignal wready
             wv  <- unsafeFreezeSignal s_axi_wvalid
             bv  <- unsafeFreezeSignal bvalid
             br  <- unsafeFreezeSignal s_axi_bready
             ifE ( isHigh awr `and`
                   isHigh awv `and`
                   isHigh wr  `and`
                   isHigh wv  `and`
                   isLow bv
                 , do bvalid <== high
                      bresp  <== zeroes)
                 ( isHigh br  `and`
                   isHigh bv
                 , do bvalid <== low))

       ----------------------------------------
       -- AXI_AWREADY generation.
       --
       processR []
         (do arready <== low
             araddr  <== ones)
         (do arr <- unsafeFreezeSignal arready
             arv <- unsafeFreezeSignal s_axi_arvalid
             iff (isLow arr `and` isHigh arv)
               (do arready <== high
                   araddr  <=- s_axi_araddr)
               (do arready <== low))

       ----------------------------------------
       -- AXI_ARVALID generation.
       --
       processR []
         (do rvalid <== low
             rresp  <== zeroes)
         (do arr <- unsafeFreezeSignal arready
             arv <- unsafeFreezeSignal s_axi_arvalid
             rv  <- unsafeFreezeSignal rvalid
             rr  <- unsafeFreezeSignal s_axi_rready
             ifE ( isHigh arr `and` isHigh arv `and` isLow rv
                 , do rvalid <== high
                      rresp  <== zeroes)
                 ( isHigh rv `and` isHigh rr
                 , do rvalid <== low))

       ----------------------------------------
       -- Memory mapped rigister select and
       -- read logic generaiton.
       --
       process (araddr .: mOutputs) (mWrite)

       ----------------------------------------
       -- Output register of memory read data.
       --
       processR []
         (do rdata <== zeroes)
         (do rden <- unsafeFreezeSignal reg_rden
             when (isHigh rden)
               (do rdata <=- reg_out))

       ----------------------------------------
       -- User logic.
       --
       portmap comp registers
       --
       -- The end.
       ----------------------------------------
  where
    -- Application-specific design signals. The first depends on the bus width,
    -- the second on the number of bits needed to store an address. There's no
    -- '+ 1' on 'addr_bits' since the ranges add that by default (i.e. '0 to 0'
    -- contains one element).
    addr_lsb, addr_bits :: Integer
    addr_lsb  = 2 -- always '2' for a 32-bit bus.
    addr_bits = bits (widthOf comp) - 1
      where
        bits :: Integer -> Integer
        bits 0 = 1
        bits 1 = 1
        bits x = floor $ (1+) $ logBase 2.0 $ fromIntegral x

    -- AXI-lite constants.
    axi_data_width, axi_addr_width :: Integer
    axi_data_width = 32 -- always use 32-bit wide busses for AXI-lite.
    axi_addr_width = addr_lsb + addr_bits + 1 -- added one for consistency.

--------------------------------------------------------------------------------
-- ** Helpers.
--------------------------------------------------------------------------------

-- | Declare the registers which will be used by our AXI-lite slave to store
--   values received from the master and, once filled, as input for the comp.
declareRegisters :: forall instr (exp :: * -> *) pred m a . AXIPred instr exp pred
  => Sig  instr exp pred Identity a
  -> Prog instr exp pred (Argument pred a)
declareRegisters (Ret _) = return Nil
declareRegisters (SSig _ _ sf) =
  do s <- newSignal
     a <- declareRegisters (sf s)
     return (ASig s a)
declareRegisters (SArr _ _ l af) =
  do s <- newArray (litP (Proxy :: Proxy pred) l)
     a <- declareRegisters (af s)
     return (AArr s a)

--------------------------------------------------------------------------------

-- | Reset the input registers.
resetInputs :: forall instr (exp :: * -> *) pred m a . AXIPred instr exp pred
  => Sig  instr exp pred Identity a
  -> Argument pred a
  -> Prog instr exp pred ()
resetInputs (Ret _)           (Nil)        = return ()
resetInputs (SSig _ Out   sf) (ASig s arg) = resetInputs (sf s) arg
resetInputs (SArr _ Out _ af) (AArr a arg) = resetInputs (af a) arg
resetInputs (SSig _ In    sf) (ASig s arg) =
  do setSignal s (litP (Proxy :: Proxy pred) reset)
     resetInputs (sf s) arg
resetInputs (SArr _ In  _ af) (AArr a arg) =
  do resetArray a (litP (Proxy :: Proxy pred) reset)
     resetInputs (af a) arg

--------------------------------------------------------------------------------

-- | Reset the input registers to their previous values.
reloadInputs :: forall instr (exp :: * -> *) pred m a . AXIPred instr exp pred
  => Sig  instr exp pred Identity a
  -> Argument pred a
  -> Prog instr exp pred ()
reloadInputs (Ret _)           (Nil)        = return ()
reloadInputs (SSig _ Out   sf) (ASig s arg) = reloadInputs (sf s) arg
reloadInputs (SArr _ Out _ af) (AArr a arg) = reloadInputs (af a) arg
reloadInputs (SSig _ In    sf) (ASig (s :: Signal b) arg) =
  case witPred (Proxy :: Proxy exp) (Dict :: Dict (pred b)) of
    Dict -> do
      sv <- unsafeFreezeSignal s
      setSignal s sv
      reloadInputs (sf s) arg
reloadInputs (SArr _ In  l af) (AArr a arg) =
  do copyArray (a, lit 0) (a, lit 0) (lit $ l - 1)
     reloadInputs (af a) arg
  where
    lit :: (FreePrim exp pred, PrimType b, pred b) => b -> exp b
    lit = litP (Proxy :: Proxy pred)

--------------------------------------------------------------------------------

-- | ...
loadInputs :: forall instr (exp :: * -> *) pred a . AXIPred instr exp pred
  => Signal   (Bits 32) -- ^ Address.
  -> Signal   (Bit)     -- ^ Ready.
  -> Signal   (Bits 32) -- ^ Input.
  -> Signal   (Bits 4)  -- ^ Protected bits.
  -> Integer            -- ^ Address lsb.
  -> Integer            -- ^ Address width.
  -> Sig instr exp pred Identity a
  -> Argument pred a
  -> Prog instr exp pred ()
loadInputs waddr rwren wdata wren addr_lsb addr_bits sig arg =
  do loc   <- getBits waddr (lit addr_lsb) (lit $ addr_bits - 1)
     ready <- unsafeFreezeSignal rwren
     when (isHigh ready) $
       switched loc
         (cases 0 sig arg)
         (reloadInputs sig arg)
  where
    cases :: Integer
          -> Sig instr exp pred Identity b
          -> Argument pred b
          -> [When Integer (Prog instr exp pred)]
    cases ix (Ret _)         (Nil)        = []
    cases ix (SSig _ Out   sf) (ASig s arg) = cases (ix + 1)             (sf s) arg
    cases ix (SArr _ Out l af) (AArr a arg) = cases (ix + P.toInteger l) (af a) arg
    cases ix (SSig _ In    sf) (ASig s arg) =
      let new = is (ix) (loadInputSignal wdata wren s)
       in new : cases (ix + 1) (sf s) arg
    cases ix (SArr _ In  l af) (AArr a arg) =
      let len = P.toInteger l
          new = map (\(i, j) -> is i (loadInputArray wdata wren a j)) $ zip [ix..ix+len-1] [0..]
       in new ++ cases (ix + len) (af a) arg

    lit :: (FreePrim exp pred, PrimType b, pred b) => b -> exp b
    lit = litP (Proxy :: Proxy pred)

loadInputSignal :: forall instr (exp :: * -> *) pred a .
     (AXIPred instr exp pred, pred a, Sized a)
  => Signal (Bits 32)
  -> Signal (Bits 4)
  -> Signal a
  -> Prog instr exp pred ()
loadInputSignal wdata wren reg = for 0 size $ \byte_index ->
  do bit <- getBit wren byte_index
     when (isHigh bit) $
       copyBits (reg, byte_index*8) (wdata, byte_index*8) (lit 7)
  where
    size :: exp Integer
    size = lit $ (P.div (bits reg) 8) - 1
    -- todo: I assume that `a` has a type \width\ that is some multiple of eight.

    lit :: (FreePrim exp pred, PrimType b, pred b) => b -> exp b
    lit = litP (Proxy :: Proxy pred)

loadInputArray :: forall instr (exp :: * -> * ) pred i a .
     (AXIPred instr exp pred, pred a, Sized a)
  => Signal (Bits 32)
  -> Signal (Bits 4)
  -> Array a
  -> Integer
  -> Prog instr exp pred ()
loadInputArray wdata wren arr ix = for 0 size $ \byte_index ->
  do bit <- getBit wren byte_index
     when (isHigh bit) $
       copyABits (arr, byte_index*8, lit ix) (wdata, byte_index*8) (lit 7)
  where
    size :: exp Integer
    size = lit $ (P.div (bits arr) 8) - 1
    -- todo: I assume that `a` has a type \width\ that is some multiple of eight.

    lit :: (FreePrim exp pred, PrimType b, pred b) => b -> exp b
    lit = litP (Proxy :: Proxy pred)

--------------------------------------------------------------------------------

-- | ...
loadOutputs :: forall instr (exp :: * -> *) pred a . AXIPred instr exp pred
  => Signal (Bits 32) -- ^ Address.
  -> Signal (Bits 32) -- ^ Output.
  -> Integer          -- ^ Address lsb.
  -> Integer          -- ^ Address width.
  -> Sig instr exp pred Identity a
  -> Argument pred a
  -> Prog instr exp pred ()
loadOutputs araddr rout addr_lsb addr_bits sig arg =
  do loc <- getBits araddr (lit addr_lsb) (lit $ addr_bits - 1)
     switched loc
       (cases 0 sig arg)
       (resetOut)
  where
    cases :: Integer
          -> Sig instr exp pred Identity b
          -> Argument pred b
          -> [When Integer (Prog instr exp pred)]
-- todo: due to a bug in the Xilinx tools I use (Vivado 2015.4), we cannot skip
-- any cases. So even if a signal is tagged as an input singal, it must be
-- part of the generated case statement to avoid holes. That is, we can't have:
--    cases ix (Ret _) (Nil) = []
--    cases ix (SSig _ In    sf) (ASig s arg) = cases (ix + 1)             (sf s) arg
--    cases ix (SArr _ In  l af) (AArr a arg) = cases (ix + P.toInteger l) (af a) arg
    cases ix (Ret _) (Nil)
      | ix < addr_max = to ix addr_max (resetOut) : []
      | otherwise     = []
    cases ix (SSig _ In    sf) (ASig s arg) =
      is ix (resetOut) : cases (ix + 1) (sf s) arg
    cases ix (SArr _ In  l af) (AArr a arg) = let len = P.toInteger l in
      to ix (ix + len - 1) (resetOut) : cases (ix + len) (af a) arg
    cases ix (SSig _ Out   sf) (ASig s arg) =
      let new = is ix (loadOutputSignal rout s)
       in new : cases (ix + 1) (sf s) arg
    cases ix (SArr _ Out l af) (AArr a arg) =
      let len = P.toInteger l
          new = map (\(i, j) -> is i (loadOutputArray rout a j)) $ zip [ix..ix+len-1] [0..]
       in new ++ cases (ix + len) (af a) arg

    lit :: (FreePrim exp pred, PrimType b, pred b) => b -> exp b
    lit = litP (Proxy :: Proxy pred)

    addr_max :: Integer
    addr_max = addr_bits ^ 2 - 1

    resetOut :: Prog instr exp pred ()
    resetOut = setSignal rout (lit reset)

loadOutputSignal :: forall instr (exp :: * -> *) pred a .
     (AXIPred instr exp pred, pred a, PrimType a, Integral a)
  => Signal (Bits 32)
  -> Signal a
  -> Prog instr exp pred ()
loadOutputSignal rout reg =
  case witPred (Proxy :: Proxy exp) (Dict :: Dict (pred a)) of
    Dict -> do
      r <- unsafeFreezeSignal reg
      setSignal rout (toBits r :: exp (Bits 32))

loadOutputArray :: forall instr (exp :: * -> *) pred a .
     ( AXIPred instr exp pred, pred a, PrimType a, Integral a)
  => Signal (Bits 32)
  -> Array a
  -> Integer
  -> Prog instr exp pred ()
loadOutputArray rout arr ix =
  case witPred (Proxy :: Proxy exp) (Dict :: Dict (pred a)) of
    Dict -> do
      r <- getArray arr (lit $ P.fromInteger ix)
      setSignal rout (toBits r :: exp (Bits 32))
  where
    lit :: (FreePrim exp pred, PrimType b, pred b) => b -> exp b
    lit = litP (Proxy :: Proxy pred)

--------------------------------------------------------------------------------

identInputs :: forall instr (exp :: * -> *) pred m a .
     Sig instr exp pred m a
  -> Argument pred a
  -> [Ident]
identInputs (Ret _)          (Nil)        = []
identInputs (SSig _ In sf)   (ASig s arg) = toIdent s : identInputs (sf s) arg
identInputs (SSig _ _  sf)   (ASig s arg) = identInputs (sf s) arg
identInputs (SArr _ In _ af) (AArr a arg) = toIdent a : identInputs (af a) arg
identInputs (SArr _ _  _ af) (AArr a arg) = identInputs (af a) arg

identOutputs :: forall instr (exp :: * -> *) pred m a .
     Sig instr exp pred m a
  -> Argument pred a
  -> [Ident]
identOutputs (Ret _) (Nil) = []
identOutputs (SSig _ Out sf)   (ASig s arg) = toIdent s : identOutputs (sf s) arg
identOutputs (SSig _ _   sf)   (ASig s arg) = identOutputs (sf s) arg
identOutputs (SArr _ Out _ af) (AArr a arg) = toIdent a : identOutputs (af a) arg
identOutputs (SArr _ _   _ af) (AArr a arg) = identOutputs (af a) arg

--------------------------------------------------------------------------------

signatureOf
  :: Comp instr exp pred m sig
  -> Sig  instr exp pred m sig
signatureOf (Component _ _ sig) = sig

widthOf
  :: Comp instr exp pred m sig
  -> Integer
widthOf comp = go (signatureOf comp)
  where
    go :: Sig instr exp pred m b -> Integer
    go (Ret _)        = 0
    go (SSig _ _ f)   = 1             + go (f dummy)
    go (SArr _ _ l g) = P.toInteger l + go (g dummy)

dummy :: a
dummy = error "todo: evaluated dummy"

high, low :: Expr exp => exp Bit
high = true
low  = false

isHigh, isLow :: (Expr exp, Rel exp) => exp Bit -> exp Bit
isHigh e = e `eq` high
isLow  e = e `eq` low

zeroes :: (Primary exp, Typeable n, KnownNat n) => exp (Bits n)
zeroes = value 0

ones :: forall exp n. (Primary exp, Typeable n, KnownNat n) => exp (Bits n)
ones = value $ bitFromInteger (2 ^ size - 1)
  where size = fromIntegral (ni (Proxy::Proxy n))

--------------------------------------------------------------------------------
