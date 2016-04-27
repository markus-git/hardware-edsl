{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE TypeOperators       #-}

module Language.Embedded.Hardware.AXI.Controller where

import Language.Embedded.Hardware

import Control.Monad.Identity (Identity)
import Control.Monad.Operational.Higher hiding (when)
import Data.Int
import Data.Word
import Data.Bits ()
import Data.Ix (Ix)

import GHC.TypeLits

import Prelude hiding (not, and, or, div, null)

--------------------------------------------------------------------------------
-- * AXI-light Controller.
--------------------------------------------------------------------------------

-- | ...
type HSig  instr = Sig instr HExp HType Identity

-- | ...
type HProg instr = ProgramT instr (Param2 HExp HType) Identity

--------------------------------------------------------------------------------
-- ** Signature.

axi_light_signature
  :: ( SignalCMD      :<: instr,
       ConstantCMD    :<: instr,
       ConditionalCMD :<: instr,
       StructuralCMD  :<: instr
     )
  => HSig instr (
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
  :: forall instr. (
       SignalCMD      :<: instr,
       ConstantCMD    :<: instr,
       ConditionalCMD :<: instr,
       StructuralCMD  :<: instr
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
  -> HProg instr ()
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
       awaddr  <- signal "axi_awaddr"  :: HProg instr (Signal (Bits 4))
       awready <- signal "axi_awready" :: HProg instr (Signal Bit)
       wready  <- signal "axi_wready"  :: HProg instr (Signal Bit)
       bresp   <- signal "axi_bresp"   :: HProg instr (Signal (Bits 2))
       bvalid  <- signal "axi_bvalid"  :: HProg instr (Signal Bit)
       araddr  <- signal "axi_araddr"  :: HProg instr (Signal (Bits 4))
       arready <- signal "axi_arready" :: HProg instr (Signal Bit)
       rdata   <- signal "axi_rdata"   :: HProg instr (Signal (Bits 32))
       rresp   <- signal "axi_rresp"   :: HProg instr (Signal (Bits 2))
       rvalid  <- signal "axi_rvalid"  :: HProg instr (Signal Bit)

       ----------------------------------------
       -- Application-specific design signals.
       --
       addr_lsb  <- constant "ADDR_LSB"          2 :: HProg instr (Constant Integer)
       addr_bits <- constant "OPT_MEM_ADDR_BITS" 1 :: HProg instr (Constant Integer)
       
       ----------------------------------------
       -- Signals for user logic registers.
       --
       -- *** Generate these from a signature ***
       --
       reg_0     <- signal "slv_reg0"     :: HProg instr (Signal (Bits 32))
       reg_1     <- signal "slv_reg1"     :: HProg instr (Signal (Bits 32))
       reg_2     <- signal "slv_reg2"     :: HProg instr (Signal (Bits 32))
       reg_3     <- signal "slv_reg3"     :: HProg instr (Signal (Bits 32))
       reg_rden  <- signal "slv_reg_rden" :: HProg instr (Signal Bit)
       reg_wren  <- signal "slv_reg_wren" :: HProg instr (Signal Bit)
       reg_out   <- signal "reg_data_out" :: HProg instr (Signal (Bits 32))
       reg_index <- signal "byte_index"   :: HProg instr (Signal Integer)

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
             (do awaddr <== (others low))
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
             (do reg_0 <== others low
                 reg_1 <== others low
                 reg_2 <== others low
                 reg_3 <== others low)
             (do clsb  <- getConstant addr_lsb
                 cbits <- getConstant addr_bits
                 slice <- undefined --getSlice awaddr (R :: Range 1 (1 + 2))
                 wren  <- get reg_wren
                 when (isHigh wren) $ do
                   i <- undefined --integer slice
                   switched (low) []
                     (do reg_0 <=- reg_0
                         reg_1 <=- reg_1
                         reg_2 <=- reg_2
                         reg_3 <=- reg_3))



       undefined
  where
    get :: HType a => Signal a -> HProg instr (HExp a)
    get = unsafeFreezeSignal

    high, low :: HExp Bit
    high = true
    low  = false

    isHigh, isLow :: HExp Bit -> HExp Bit
    isHigh = undefined
    isLow  = undefined

--------------------------------------------------------------------------------
