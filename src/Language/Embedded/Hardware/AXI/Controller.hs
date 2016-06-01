{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE PolyKinds           #-}

module Language.Embedded.Hardware.AXI.Controller where

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

type Prog instr exp pred = Program instr (Param2 exp pred)

--------------------------------------------------------------------------------
-- ** Signature.

axi_light_signature
  :: forall instr exp pred. (
       SignalCMD      :<: instr
     , ConstantCMD    :<: instr
     , ArrayCMD       :<: instr
     , ConditionalCMD :<: instr
     , StructuralCMD  :<: instr
     , LoopCMD        :<: instr
     , Hardware exp
     , FreeExp exp
     , pred (Bit),     PredicateExp exp (Bit)
     , pred (Bits 2),  PredicateExp exp (Bits 2)
     , pred (Bits 3),  PredicateExp exp (Bits 3)
     , pred (Bits 4),  PredicateExp exp (Bits 4)
     , pred (Bits 32), PredicateExp exp (Bits 32)
     , pred (UBits),   PredicateExp exp (UBits)
     , pred (Integer), PredicateExp exp (Integer)
     , Num (exp Integer)
     )
  => Sig instr exp pred Identity (
          Signal Bit       -- ^ Global clock signal.
       -> Signal Bit       -- ^ Global reset signal.
       -> Signal (Bits 4)  -- ^ Write address.
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
       -> Signal (Bits 4)  -- ^ Read address.
       -> Signal (Bits 3)  -- ^ Protection type.
       -> Signal Bit       -- ^ Read address valid.
       -> Signal Bit       -- ^ Read address ready.
       -> Signal (Bits 32) -- ^ Read data.
       -> Signal (Bits 2)  -- ^ Read response.
       -> Signal Bit       -- ^ Read valid.
       -> Signal Bit       -- ^ Read ready.    
       -> ()
     )
axi_light_signature =
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
  ret $ axi_light
    s_axi_aclk s_axi_aresetn
    s_axi_awaddr s_axi_awprot s_axi_awvalid s_axi_awready s_axi_wdata s_axi_wstrb s_axi_wvalid s_axi_wready
    s_axi_bresp  s_axi_bvalid s_axi_bready
    s_axi_araddr s_axi_arprot s_axi_arvalid s_axi_arready s_axi_rdata
    s_axi_rresp  s_axi_rvalid s_axi_rready     

--------------------------------------------------------------------------------
-- ** Implementation.

axi_light
  :: forall instr exp pred n. (
       SignalCMD      :<: instr
     , ConstantCMD    :<: instr
     , ArrayCMD       :<: instr
     , ConditionalCMD :<: instr
     , StructuralCMD  :<: instr
     , LoopCMD        :<: instr
     , Hardware exp
     , FreeExp exp
     , pred (Bit),     PredicateExp exp (Bit)
     , pred (Bits 2),  PredicateExp exp (Bits 2)
     , pred (Bits 3),  PredicateExp exp (Bits 3)
     , pred (Bits 4),  PredicateExp exp (Bits 4)
     , pred (Bits 32), PredicateExp exp (Bits 32)
     , pred (UBits),   PredicateExp exp (UBits)
     , pred (Integer), PredicateExp exp (Integer)
     , Num (exp Integer)
     )
  => Signal Bit       -- ^ Global clock signal.
  -> Signal Bit       -- ^ Global reset signal.
  -> Signal (Bits 4)  -- ^ Write address.
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
  -> Signal (Bits 4)  -- ^ Read address.
  -> Signal (Bits 3)  -- ^ Protection type.
  -> Signal Bit       -- ^ Read address valid.
  -> Signal Bit       -- ^ Read address ready.
  -> Signal (Bits 32) -- ^ Read data.
  -> Signal (Bits 2)  -- ^ Read response.
  -> Signal Bit       -- ^ Read valid.
  -> Signal Bit       -- ^ Read ready.    
  -> Program instr (Param2 exp pred) ()
axi_light
    s_axi_aclk   s_axi_aresetn
    s_axi_awaddr s_axi_awprot s_axi_awvalid s_axi_awready s_axi_wdata s_axi_wstrb s_axi_wvalid s_axi_wready
    s_axi_bresp  s_axi_bvalid s_axi_bready
    s_axi_araddr s_axi_arprot s_axi_arvalid s_axi_arready s_axi_rdata
    s_axi_rresp  s_axi_rvalid s_axi_rready
  = do

       ----------------------------------------
       -- AXI Light signals.
       --
       awaddr  <- signal "axi_awaddr"              :: Prog instr exp pred (Signal (Bits 4))
       awready <- signal "axi_awready"             :: Prog instr exp pred (Signal (Bit))
       wready  <- signal "axi_wready"              :: Prog instr exp pred (Signal (Bit))
       bresp   <- signal "axi_bresp"               :: Prog instr exp pred (Signal (Bits 2))
       bvalid  <- signal "axi_bvalid"              :: Prog instr exp pred (Signal (Bit))
       araddr  <- signal "axi_araddr"              :: Prog instr exp pred (Signal (Bits 4))
       arready <- signal "axi_arready"             :: Prog instr exp pred (Signal (Bit))
       rdata   <- signal "axi_rdata"               :: Prog instr exp pred (Signal (Bits 32))
       rresp   <- signal "axi_rresp"               :: Prog instr exp pred (Signal (Bits 2))
       rvalid  <- signal "axi_rvalid"              :: Prog instr exp pred (Signal (Bit))

       ----------------------------------------
       -- Application-specific design signals.
       --
       addr_lsb  <- constant "ADDR_LSB"          2 :: Prog instr exp pred (Constant Integer)
       addr_bits <- constant "OPT_MEM_ADDR_BITS" 1 :: Prog instr exp pred (Constant Integer)

       ----------------------------------------
       -- Signals for user logic registers.
       --
       -- *** ToDo: Generate these from a signature
       --
       reg_0     <- signal "slv_reg0"              :: Prog instr exp pred (Signal (Bits 32))
       reg_1     <- signal "slv_reg1"              :: Prog instr exp pred (Signal (Bits 32))
       reg_2     <- signal "slv_reg2"              :: Prog instr exp pred (Signal (Bits 32))
       reg_3     <- signal "slv_reg3"              :: Prog instr exp pred (Signal (Bits 32))
       reg_rden  <- signal "slv_reg_rden"          :: Prog instr exp pred (Signal (Bit))
       reg_wren  <- signal "slv_reg_wren"          :: Prog instr exp pred (Signal (Bit))
       reg_out   <- signal "reg_data_out"          :: Prog instr exp pred (Signal (Bits 32))
       reg_index <- signal "byte_index"            :: Prog instr exp pred (Signal (Integer))

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
         clk <- get s_axi_aclk
         rst <- get s_axi_aresetn
         when (risingEdge clk) $ do
           iff (isLow rst)
             (do awready <== low)
             (do rdy <- get awready
                 awv <- get s_axi_awvalid
                 wv  <- get s_axi_wvalid
                 iff (isLow  rdy `and`
                      isHigh awv `and`
                      isHigh wv)
                   (do awready <== high)
                   (do awready <== low))

       ----------------------------------------
       -- AXI_AWADDR latching.
       --
       process (s_axi_aclk .: []) $ do
         clk <- get s_axi_aclk
         rst <- get s_axi_aresetn
         when (risingEdge clk) $ do
           iff (isLow rst)
             (do awaddr <== zeroes)
             (do rdy <- get awready
                 awv <- get s_axi_awvalid
                 wv  <- get s_axi_wvalid
                 when (isLow  rdy `and`
                       isHigh awv `and`
                       isHigh wv) $
                   awaddr <=- s_axi_awaddr)

       ----------------------------------------
       -- AXI_AWREADY generation.
       --
       process (s_axi_aclk .: []) $ do
         clk <- get s_axi_aclk
         rst <- get s_axi_aresetn
         when (risingEdge clk) $ do
           iff (isLow rst)
             (do wready <== low)
             (do rdy <- get awready
                 awv <- get s_axi_awvalid
                 wv  <- get s_axi_wvalid
                 iff (isLow  rdy `and`
                      isHigh awv `and`
                      isHigh wv)
                   (wready <== high)
                   (wready <== low))

       ----------------------------------------
       -- Slave register logic.
       --
       process (s_axi_aclk .: []) $ do
         clk   <- get s_axi_aclk
         rst   <- get s_axi_aresetn
         when (risingEdge clk) $ do
           iff (isLow rst)
             (do reg_0 <== zeroes
                 reg_1 <== zeroes
                 reg_2 <== zeroes
                 reg_3 <== zeroes)
             (do clsb     <- getConstant addr_lsb
                 cbits    <- getConstant addr_bits
                 loc_addr <- getSignalRange cbits (clsb + cbits, clsb) awaddr
                 rwren    <- get reg_wren
                 when (isHigh rwren) $
                   do switched loc_addr
                        [ is (fromInteger 0) $ for (value (3 :: Integer)) $ \ix ->
                            do b <- getArray ix s_axi_wstrb
                               when (isHigh b) $
                                 setSignalRange
                                   (ix*8+7, ix*8) reg_0
                                   (ix*8+7, ix*8) s_axi_wdata
                        , is (fromInteger 1) $ for (value (3 :: Integer)) $ \ix ->
                            do b <- getArray ix s_axi_wstrb
                               when (isHigh b) $
                                 setSignalRange
                                   (ix*8+7, ix*8) reg_1
                                   (ix*8+7, ix*8) s_axi_wdata
                        , is (fromInteger 2) $ for (value (3 :: Integer)) $ \ix ->
                            do b <- getArray ix s_axi_wstrb
                               when (isHigh b) $
                                 setSignalRange
                                   (ix*8+7, ix*8) reg_2
                                   (ix*8+7, ix*8) s_axi_wdata
                        , is (fromInteger 3) $ for (value (3 :: Integer)) $ \ix ->
                            do b <- getArray ix s_axi_wstrb
                               when (isHigh b) $
                                 setSignalRange
                                   (ix*8+7, ix*8) reg_3
                                   (ix*8+7, ix*8) s_axi_wdata
                        ]
                        (do reg_0 <=- reg_0
                            reg_1 <=- reg_1
                            reg_2 <=- reg_2
                            reg_3 <=- reg_3))

       ----------------------------------------
       -- Write response logic.
       --
       process (s_axi_aclk .: []) $ do
         clk   <- get s_axi_aclk
         rst   <- get s_axi_aresetn
         when (risingEdge clk) $ do
           iff (isLow rst)
             (do bvalid <== low
                 bresp  <==  zeroes)
             (do awr <- get awready
                 awv <- get s_axi_awvalid
                 wr  <- get wready
                 wv  <- get s_axi_wvalid
                 bv  <- get bvalid
                 br  <- get s_axi_bready
                 ifE (isHigh awr `and` isHigh awv `and` isHigh wr  `and` isHigh wv  `and` isLow bv,
                   do bvalid <== high
                      bresp  <== zeroes)
                     (isHigh br `and` isHigh bv,
                   do setSignal bvalid low))

       ----------------------------------------
       -- AXI_AWREADY generation.
       --
       process (s_axi_aclk .: []) $ do
         clk   <- get s_axi_aclk
         rst   <- get s_axi_aresetn
         when (risingEdge clk) $ do
           iff (isLow rst)
             (do arready <== low
                 araddr  <== ones)
             (do arr <- get arready
                 arv <- get s_axi_arvalid
                 iff (isLow arr `and` isHigh arv)
                   (do arready <== high
                       araddr  <=- s_axi_araddr)
                   (do arready <== low))

       ----------------------------------------
       -- AXI_ARVALID generation.
       --
       process (s_axi_aclk .: []) $ do
         clk   <- get s_axi_aclk
         rst   <- get s_axi_aresetn
         when (risingEdge clk) $ do
           iff (isLow rst)
             (do rvalid <== low
                 rresp  <== zeroes)
             (do arr <- get arready
                 arv <- get s_axi_arvalid
                 rv  <- get rvalid
                 rr  <- get s_axi_rready
                 ifE (isHigh arr `and` isHigh arv,
                   do rvalid <== high
                      rresp  <== zeroes)
                     (isHigh rv  `and` isHigh rr,
                   do rvalid <== low))

       ----------------------------------------
       -- Memory mapped rigister select and
       -- read logic generaiton.
       --
       process (reg_0 .: reg_1 .: reg_2 .: reg_3 .: araddr .: s_axi_aresetn .: reg_rden .: []) $ do
         clsb     <- getConstant addr_lsb
         cbits    <- getConstant addr_bits
         loc_addr <- getSignalRange cbits (clsb + cbits, clsb) araddr
         switched loc_addr
           [ is (fromInteger 0) $
               do reg_out <=- reg_0
           , is (fromInteger 1) $
               do reg_out <=- reg_1
           , is (fromInteger 2) $
               do reg_out <=- reg_2
           , is (fromInteger 3) $
               do reg_out <=- reg_3
           ]
           (do reg_out <== zeroes)

       ----------------------------------------
       -- Output register of memory read data.
       --
       process (s_axi_aclk .: []) $ do
         clk   <- get s_axi_aclk
         rst   <- get s_axi_aresetn
         when (risingEdge clk) $ do
           iff (isLow rst)
             (rdata <== zeroes)
             (do rden <- get reg_rden
                 when (isHigh rden) $
                   rdata <=- reg_out)

--------------------------------------------------------------------------------

type HCMD =
      SignalCMD
  :+: StructuralCMD
  :+: ConditionalCMD
  :+: ConstantCMD
  :+: ComponentCMD
  :+: ArrayCMD
  :+: LoopCMD

axi :: Prog HCMD HExp HType ()
axi = do c <- component axi_light_signature
         return ()

test = icompile axi

--------------------------------------------------------------------------------

get
  :: (PredicateExp exp a, pred a, FreeExp exp, SignalCMD :<: instr)
  => Signal a -> Prog instr exp pred (exp a)
get = unsafeFreezeSignal

high, low :: Expr exp => exp Bit
high = true
low  = false

isHigh, isLow :: (Expr exp, Rel exp) => exp Bit -> exp Bit
isHigh = flip eq high
isLow  = flip eq low

zeroes :: (Value exp, Typeable n, KnownNat n) => exp (Bits n)
zeroes = value 0

ones   :: forall exp n. (Value exp, Typeable n, KnownNat n) => exp (Bits n)
ones   = value $ bitFromInteger (read (concat $ take size $ repeat "1") :: Integer)
  where size = fromIntegral (ni (Proxy::Proxy n)) - 1

--------------------------------------------------------------------------------
