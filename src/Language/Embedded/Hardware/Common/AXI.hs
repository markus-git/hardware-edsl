{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE GADTs               #-}

module Language.Embedded.Hardware.Common.AXI where

import Language.Embedded.Hardware

import Control.Monad.Identity (Identity)
import Control.Monad.Operational.Higher hiding (when)
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
--
-- todo : we make a slight simplification and assume that components which we
--        connect to AXI-lite has a signature of
--          "input -> input -> .. -> input -> output -> ()"
--        this can easily be fixed by inspecting the modes given by the
--        signature.

-- | Short-hand for programs.
type Prog instr exp pred = Program instr (Param2 exp pred)

--------------------------------------------------------------------------------
-- ** Signature.

axi_light_signature
  :: forall instr exp pred sig. (
       SignalCMD      :<: instr
     , ConstantCMD    :<: instr
     , ArrayCMD       :<: instr
     , ConditionalCMD :<: instr
     , StructuralCMD  :<: instr
     , LoopCMD        :<: instr
     , VHDLCMD        :<: instr
     , Hardware exp
     , FreeExp exp
     , pred (Bit),     PredicateExp exp (Bit)
     , pred (Bits 2),  PredicateExp exp (Bits 2)
     , pred (Bits 3),  PredicateExp exp (Bits 3)
     , pred (Bits 4),  PredicateExp exp (Bits 4)
     , pred (Bits 32), PredicateExp exp (Bits 32)
--   , pred (UBits),   PredicateExp exp (UBits)
     , pred (Integer), PredicateExp exp (Integer)
     , Num (exp Integer)
     )
  => Sig instr exp pred Identity (sig)
  -> Sig instr exp pred Identity (
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
axi_light_signature sig =
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
  ret $ axi_light sig
    s_axi_aclk s_axi_aresetn
    s_axi_awaddr s_axi_awprot s_axi_awvalid s_axi_awready
    s_axi_wdata  s_axi_wstrb  s_axi_wvalid  s_axi_wready
    s_axi_bresp  s_axi_bvalid s_axi_bready
    s_axi_araddr s_axi_arprot s_axi_arvalid s_axi_arready s_axi_rdata
    s_axi_rresp  s_axi_rvalid s_axi_rready     

--------------------------------------------------------------------------------
-- ** Implementation.

axi_light
  :: forall instr exp pred sig. (
       SignalCMD      :<: instr
     , ConstantCMD    :<: instr
     , ArrayCMD       :<: instr
     , ConditionalCMD :<: instr
     , StructuralCMD  :<: instr
     , LoopCMD        :<: instr
     , VHDLCMD        :<: instr
     , Hardware exp
     , FreeExp exp     
     , pred (Bit),     PredicateExp exp (Bit)
     , pred (Bits 2),  PredicateExp exp (Bits 2)
     , pred (Bits 3),  PredicateExp exp (Bits 3)
     , pred (Bits 4),  PredicateExp exp (Bits 4)
     , pred (Bits 32), PredicateExp exp (Bits 32)
--   , pred (UBits),   PredicateExp exp (UBits)
     , pred (Integer), PredicateExp exp (Integer)
     , Num (exp Integer)
     )
  => Sig instr exp pred Identity sig
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
  -> Program instr (Param2 exp pred) ()
axi_light sig
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
       registers <- sequence [ reg i | i <- [0 .. width - 1]]
                      :: Prog instr exp pred [Signal (Bits 32)]
       
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
       -- AXI_AWREADY generation.
       --
       process (s_axi_aclk .: []) $ do
         whenRising s_axi_aclk s_axi_aresetn
           (do awready <== low)
           (do rdy <- getSignal awready
               awv <- getSignal s_axi_awvalid
               wv  <- getSignal s_axi_wvalid
               iff (isLow rdy `and` isHigh awv `and` isHigh wv)
                 (do awready <== high)
                 (do awready <== low))
           
       ----------------------------------------
       -- AXI_AWADDR latching.
       --
       process (s_axi_aclk .: []) $ do
         whenRising s_axi_aclk s_axi_aresetn
           (do awaddr <== zeroes)
           (do rdy <- getSignal awready
               awv <- getSignal s_axi_awvalid
               wv  <- getSignal s_axi_wvalid
               when (isLow  rdy `and` isHigh awv `and` isHigh wv)
                 (awaddr <=- s_axi_awaddr))

       ----------------------------------------
       -- AXI_WREADY generation.
       --
       process (s_axi_aclk .: []) $ do
         whenRising s_axi_aclk s_axi_aresetn
           (do wready <== low)
           (do rdy <- getSignal awready
               awv <- getSignal s_axi_awvalid
               wv  <- getSignal s_axi_wvalid
               iff (isLow  rdy `and` isHigh awv `and` isHigh wv)
                 (wready <== high)
                 (wready <== low))

       ----------------------------------------
       -- Slave register logic.
       --
       process (s_axi_aclk .: []) $ do
         whenRising s_axi_aclk s_axi_aresetn
           (do sequence_ [ reg <== zeroes | reg <- registers ])
           (do loc_addr <- getBits awaddr (value addr_lsb) (value addr_bits)
               rwren    <- getSignal reg_wren
               when (isHigh rwren) $ switched loc_addr
                 [ is i $ for 0 3 $ \ix ->
                     do wb <- getBit s_axi_wstrb ix
                        when (isHigh wb) $ 
                          copyBits
                            (registers !! fromIntegral i, ix*8)
                            (s_axi_wdata, ix*8)
                            (ix*8 + 7)
                 | i <- [0..width]
                 ]
                 (do sequence_ [ reg <=- reg | reg <- registers ]))

       ----------------------------------------
       -- Write response logic.
       --
       process (s_axi_aclk .: []) $ do
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
                     do bvalid <== low))

       ----------------------------------------
       -- AXI_AWREADY generation.
       --
       process (s_axi_aclk .: []) $ do
         whenRising s_axi_aclk s_axi_aresetn
           (do arready <== low
               araddr  <== ones)
           (do arr <- getSignal arready
               arv <- getSignal s_axi_arvalid
               iff (isLow arr `and` isHigh arv)
                 (do arready <== high
                     araddr  <=- s_axi_araddr)
                 (do arready <== low))

       ----------------------------------------
       -- AXI_ARVALID generation.
       --
       process (s_axi_aclk .: []) $ do
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
                     do rvalid <== low))

       ----------------------------------------
       -- Memory mapped rigister select and
       -- read logic generaiton.
       --
       process (araddr .: s_axi_aresetn .: reg_rden .: map toIdent registers) $ do
         loc_addr <- getBits araddr (value addr_lsb) (value addr_bits)
         switched loc_addr
           [ is i $ reg_out <=- (registers !! fromInteger i)
           | i <- [0..width]
           ]
           (sequence_ [ reg <=- reg | reg <- registers ])

       ----------------------------------------
       -- Output register of memory read data.
       --
       process (s_axi_aclk .: []) $ do
         whenRising s_axi_aclk s_axi_aresetn
           (do rdata <== zeroes)
           (do rden <- getSignal reg_rden
               when (isHigh rden)
                 (do rdata <=- reg_out))
  where
    width :: Integer
    width = widthOf sig
    
    -- Application-specific design signals.
    addr_lsb, addr_bits :: Integer
    addr_lsb  = 2
    addr_bits = addr_lsb + width

    reg :: Integer -> Prog instr exp pred (Signal (Bits 32))
    reg i = signal ("slv_reg" ++ show i)

--------------------------------------------------------------------------------
-- ** ...

widthOf :: Signature fs a -> Integer
widthOf (Ret _)      = 0
widthOf (SSig _ _ f) = 1 + widthOf (f dummy)
widthOf (SArr _ _ g) = 1 + widthOf (g dummy)

inputs :: Sig instr exp pred Identity sig -> [Ident]
inputs (Ret _)        = []
inputs (SSig n In sf) = undefined
inputs (SSig _ _  sf) = inputs (sf dummy)

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
