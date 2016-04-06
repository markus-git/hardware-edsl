{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE TypeOperators       #-}

module Language.Embedded.Hardware.AXI.Controller where

import Language.Embedded.Hardware

import Control.Monad.Identity (Identity)
import Control.Monad.Operational.Higher
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
  :: ( SignalCMD   :<: instr,
       ConstantCMD :<: instr
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
  uniqueInput  "S_AXI_ACLK"    $ \s_axi_aclk    ->       
  uniqueInput  "S_AXI_ARESETN" $ \s_axi_aresetn -> 
  uniqueInput  "S_AXI_AWADDR"  $ \s_axi_awaddr  ->
  uniqueInput  "S_AXI_AWPROT"  $ \s_axi_awprot  ->
  uniqueInput  "S_AXI_AWVALID" $ \s_axi_awvalid -> 
  uniqueOutput "S_AXI_AWREADY" $ \s_axi_awready ->
  uniqueInput  "S_AXI_WDATA"   $ \s_axi_wdata   ->
  uniqueInput  "S_AXI_WSTRB"   $ \s_axi_wstrb   ->
  uniqueInput  "S_AXI_WVALID"  $ \s_axi_wvalid  ->   
  uniqueOutput "S_AXI_WREADY"  $ \s_axi_wready  ->   
  uniqueOutput "S_AXI_BRESP"   $ \s_axi_bresp   ->     
  uniqueOutput "S_AXI_BVALID"  $ \s_axi_bvalid  ->   
  uniqueInput  "S_AXI_BREADY"  $ \s_axi_bready  ->   
  uniqueInput  "S_AXI_ARADDR"  $ \s_axi_araddr  ->
  uniqueInput  "S_AXI_ARPROT"  $ \s_axi_arprot  ->
  uniqueInput  "S_AXI_ARVALID" $ \s_axi_arvalid ->   
  uniqueOutput "S_AXI_ARREADY" $ \s_axi_arready ->
  uniqueOutput "S_AXI_RDATA"   $ \s_axi_rdata   ->     
  uniqueOutput "S_AXI_RRESP"   $ \s_axi_rresp   ->     
  uniqueOutput "S_AXI_RVALID"  $ \s_axi_rvalid  ->
  uniqueInput  "S_AXI_RREADY"  $ \s_axi_rready  ->   
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
       SignalCMD   :<: instr,
       ConstantCMD :<: instr
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

       undefined

--------------------------------------------------------------------------------
