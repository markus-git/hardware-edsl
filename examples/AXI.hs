{-# LANGUAGE TypeOperators #-}

module AXI where

import Language.VHDL (Mode(..))
import Language.Embedded.Hardware

import Control.Monad.Identity           (Identity)
import Control.Monad.Operational.Higher ((:+:), Program)
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

--------------------------------------------------------------------------------

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
  s_axi_rdata  s_axi_rresp   s_axi_rvalid  s_axi_rready
  s_axi_bresp  s_axi_bvalid  s_axi_bready
   =
  do local_reset         <- newSignal :: P (Signal Bit)
     local_address       <- newSignal :: P (Signal Bit) -- P (Signal Int)
     local_address_valid <- newSignal :: P (Signal Bit)

     combined      <- newSignal :: P (Signal Bit2)
     write_enable  <- newSignal :: P (Signal Bit)
     send_read     <- newSignal :: P (Signal Bit)

     current_state <- newSignal :: P (Signal Word4)
     next_state    <- newSignal :: P (Signal Word4)

     -- state_machine_update
     process (s_axi_aclk .: []) $
       do clk <- unsafeFreezeSignal s_axi_aclk
          rst <- unsafeFreezeSignal local_reset
          when ((event clk) `and` (clk `eq` true)) $
            iff (rst `eq` true)
              (current_state <-- reset)
              (current_state <== next_state)

     process (current_state .: combined     .: local_address .: local_address_valid .:
              s_axi_arvalid .: s_axi_rready .: s_axi_awvalid .: s_axi_wvalid  .: s_axi_bready .:  []) $
       do s_axi_arready <-- false
          --s_axi_rresp   <-- undefined
          s_axi_rvalid  <-- false
          s_axi_wready  <-- false
          --s_axi_bresp   <-- undefined
          s_axi_bvalid  <-- false
          s_axi_wready  <-- false
          s_axi_awready <-- false
          write_enable  <-- false
          send_read     <-- false

          s <- getSignal current_state
          switch s [
            
            -- idle.
              (0, do next_state <-- idle
              )
              
            -- reset.
            , (1, do next_state <-- idle
                     c <- getSignal combined
                     switch c [
                         (1, next_state <-- reading)
                       , (2, next_state <-- writing)
                       ]
              )
            -- read transaction in progress.
            , (2, do next_state    <-- reading
                     s_axi_arready <== s_axi_arvalid
                     s_axi_rvalid  <-- litE 0
                     s_axi_rresp   <-- litE 0
                     send_read     <-- litE 1

                     rdy <- unsafeFreezeSignal s_axi_rready
                     when (rdy `eq` true) $
                       next_state <-- complete
              )

            -- write transaction in progress.
            , (3, do next_state    <-- writing
                     s_axi_awready <== s_axi_awvalid
                     s_axi_wready  <== s_axi_wvalid
                     s_axi_bresp   <-- litE 0
                     s_axi_bvalid  <-- litE 1

                     rdy <- unsafeFreezeSignal s_axi_bready
                     when (rdy `eq` true) $
                       next_state <-- complete
              )

            -- complete.
            , (4, do cbd <- unsafeFreezeSignal combined
                     switched cbd [
                       (0, next_state <-- idle)]
                       (next_state <-- complete)
              )
            ]

     return ()
   where
     reset, idle, reading, writing, complete :: HExp Word4
     reset    = litE 0
     idle     = litE 1
     reading  = litE 2
     writing  = litE 3
     complete = litE 4

(.:) :: Signal a -> [SignalX] -> [SignalX]
(.:) x xs = SignalX x : xs

infixr .:

--------------------------------------------------------------------------------

wrap :: P ()
wrap = do
  axi <- component "AXI" axi_light_signature
  inp <- structEntity "controller" $ do
    newPort "inp" In :: P (Signal Bit)
  structArchitecture "controller" "behaviour" $ do
    s_axi_aclk    <- newSignal :: P (Signal Bit) 
    s_axi_aresetn <- newSignal :: P (Signal Bit) 
    s_axi_awaddr  <- newSignal :: P (Signal Bit32) 
    s_axi_awvalid <- newSignal :: P (Signal Bit) 
    s_axi_awready <- newSignal :: P (Signal Bit)
    s_axi_araddr  <- newSignal :: P (Signal Bit32) 
    s_axi_arvalid <- newSignal :: P (Signal Bit)
    s_axi_arready <- newSignal :: P (Signal Bit) 
    s_axi_wdata   <- newSignal :: P (Signal Bit32) 
    s_axi_wstrb   <- newSignal :: P (Signal Bit4) 
    s_axi_wvalid  <- newSignal :: P (Signal Bit)
    s_axi_wready  <- newSignal :: P (Signal Bit) 
    s_axi_rdata   <- newSignal :: P (Signal Bit32)
    s_axi_rresp   <- newSignal :: P (Signal Bit2)
    s_axi_rvalid  <- newSignal :: P (Signal Bit)
    s_axi_rready  <- newSignal :: P (Signal Bit)
    s_axi_bresp   <- newSignal :: P (Signal Bit2) 
    s_axi_bvalid  <- newSignal :: P (Signal Bit)
    s_axi_bready  <- newSignal :: P (Signal Bit)
    
    portmap axi (
      s_axi_aclk    :>  
      s_axi_aresetn :>  
      s_axi_awaddr  :>  
      s_axi_awvalid :>  
      s_axi_awready :> 
      s_axi_araddr  :>  
      s_axi_arvalid :> 
      s_axi_arready :>  
      s_axi_wdata   :>  
      s_axi_wstrb   :>  
      s_axi_wvalid  :> 
      s_axi_wready  :>  
      s_axi_rdata   :> 
      s_axi_rresp   :> 
      s_axi_rvalid  :> 
      s_axi_rready  :> 
      s_axi_bresp   :>  
      s_axi_bvalid  :> 
      s_axi_bready  :>
      Nill )

test :: IO ()
test = putStrLn $ compile $ wrap

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
