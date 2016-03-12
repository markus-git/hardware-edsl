{-# LANGUAGE TypeOperators #-}

module Array where

import Language.VHDL (Mode(..))
import Language.Embedded.Hardware

import Control.Monad.Identity
import Control.Monad.Operational.Higher
import Data.ALaCarte
import Data.Int
import Data.Word
import Text.PrettyPrint

import Prelude hiding (and, or)

--------------------------------------------------------------------------------
-- *
--------------------------------------------------------------------------------

-- | Command set used for our 'simple' programs.
type CMD =
      SignalCMD       HExp
  :+: VariableCMD     HExp
  :+: ArrayCMD        HExp
  :+: VArrayCMD       HExp
  :+: LoopCMD         HExp
  :+: ConditionalCMD  HExp
  :+: ComponentCMD    HExp
  :+: StructuralCMD   HExp

-- | Short-hand for programs.
type P = Program CMD

-- | Short-hand for signatures.
type Signature = Sig HExp P

--------------------------------------------------------------------------------

axi_light_signature ::
  Signature (
       Signal Bit   -> Signal Bit
    -> Signal Bit32 -> Signal Bit   -> Signal Bit
    -> Signal Bit32 -> Signal Bit   -> Signal Bit
    -> Signal Bit32 -> Signal Bit4  -> Signal Bit  -> Signal Bit
    -> Signal Bit32 -> Signal Bit2  -> Signal Bit  -> Signal Bit
    -> Signal Bit2  -> Signal Bit   -> Signal Bit
    -> ()
  )
axi_light_signature =
  input  "S_AXI_ACLK"    $ \s_axi_aclk    ->       
  input  "S_AXI_ARESETN" $ \s_axi_aresetn -> 
  input  "S_AXI_AWADDR"  $ \s_axi_awaddr  ->  
  input  "S_AXI_AWVALID" $ \s_axi_awvalid -> 
  output "S_AXI_AWREADY" $ \s_axi_awready -> 
  input  "S_AXI_ARADDR"  $ \s_axi_araddr  ->  
  input  "S_AXI_ARVALID" $ \s_axi_arvalid -> 
  output "S_AXI_ARREADY" $ \s_axi_arready -> 
  input  "S_AXI_WDATA"   $ \s_axi_wdata   ->     
  input  "S_AXI_WSTRB"   $ \s_axi_wstrb   ->     
  input  "S_AXI_WVALID"  $ \s_axi_wvalid  ->   
  output "S_AXI_WREADY"  $ \s_axi_wready  ->   
  output "S_AXI_RDATA"   $ \s_axi_rdata   ->     
  output "S_AXI_RRESP"   $ \s_axi_rresp   ->     
  output "S_AXI_RVALID"  $ \s_axi_rvalid  ->   
  input  "S_AXI_RREADY"  $ \s_axi_rready  ->   
  output "S_AXI_BRESP"   $ \s_axi_bresp   ->     
  output "S_AXI_BVALID"  $ \s_axi_bvalid  ->   
  input  "S_AXI_BREADY"  $ \s_axi_bready  ->   
  ret $
    axi_light
      s_axi_aclk   s_axi_aresetn
      s_axi_awaddr s_axi_awvalid s_axi_awready
      s_axi_araddr s_axi_arvalid s_axi_arready
      s_axi_wdata  s_axi_wstrb   s_axi_wvalid  s_axi_wready
      s_axi_rdata  s_axi_rresp   s_axi_rvalid s_axi_rready
      s_axi_bresp  s_axi_bvalid  s_axi_bready

axi_light
  :: Signal Bit   -> Signal Bit
  -> Signal Bit32 -> Signal Bit   -> Signal Bit
  -> Signal Bit32 -> Signal Bit   -> Signal Bit
  -> Signal Bit32 -> Signal Bit4  -> Signal Bit  -> Signal Bit
  -> Signal Bit32 -> Signal Bit2  -> Signal Bit  -> Signal Bit
  -> Signal Bit2  -> Signal Bit   -> Signal Bit
  -> P ()
axi_light
  s_axi_aclk   s_axi_aresetn
  s_axi_awaddr s_axi_awvalid s_axi_awready
  s_axi_araddr s_axi_arvalid s_axi_arready
  s_axi_wdata  s_axi_wstrb   s_axi_wvalid  s_axi_wready
  s_axi_rdata  s_axi_rresp   s_axi_rvalid s_axi_rready
  s_axi_bresp  s_axi_bvalid  s_axi_bready
   =
  do
     undefined

--------------------------------------------------------------------------------

test :: IO ()
test = do
  putStrLn "\n### Simple ###\n"
  putStrLn $ undefined --compile $ axi_light

--------------------------------------------------------------------------------

{-
todo *** replace the signature crap with:

axi_light :: ...
axi_light =
  do i <- port "..." In
     o <- port "..." Out
     return $
       do ...
-}
