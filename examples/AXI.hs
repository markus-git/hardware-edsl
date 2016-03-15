{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module AXI where

import Language.VHDL (Mode(..))
import Language.Embedded.Hardware

import Control.Monad.Identity           (Identity)
import Control.Monad.Operational.Higher ((:+:), Program)
import Data.ALaCarte
import Data.Int
import Data.Word

import GHC.TypeLits

import Prelude hiding (not, and, or, div, null)

--------------------------------------------------------------------------------
-- *
--------------------------------------------------------------------------------

-- | Command set used for our 'simple' programs.
type CMD =
      SignalCMD       HExp
  :+: VariableCMD     HExp
  :+: ConstantCMD     HExp
  :+: ArrayCMD        HExp
  :+: VArrayCMD       HExp
  :+: LoopCMD         HExp
  :+: ConditionalCMD  HExp
  :+: ComponentCMD    HExp
  :+: StructuralCMD   HExp
  -- ...
  :+: IntegerCMD      HExp

-- | Short-hand for programs.
type P = Program CMD

-- | Short-hand for signatures.
type Signature = Sig HExp P

--------------------------------------------------------------------------------

-- | ...
axi_light_signature ::
  Signature (
       Signal Bool     -- ^ Global clock signal.
    -> Signal Bool     -- ^ Global reset signal.
    -> Signal (Bit 4)  -- ^ Write address.
    -> Signal (Bit 3)  -- ^ Write channel protection type.
    -> Signal Bool     -- ^ Write address valid.
    -> Signal Bool     -- ^ Write address ready.
    -> Signal (Bit 32) -- ^ Write data.
    -> Signal (Bit 4)  -- ^ Write strobes.
    -> Signal Bool     -- ^ Write valid.
    -> Signal Bool     -- ^ Write ready.
    -> Signal (Bit 2)  -- ^ Write response.
    -> Signal Bool     -- ^ Write response valid.
    -> Signal Bool     -- ^ Response ready.
    -> Signal (Bit 4)  -- ^ Read address.
    -> Signal (Bit 3)  -- ^ Protection type.
    -> Signal Bool     -- ^ Read address valid.
    -> Signal Bool     -- ^ Read address ready.
    -> Signal (Bit 32) -- ^ Read data.
    -> Signal (Bit 2)  -- ^ Read response.
    -> Signal Bool     -- ^ Read valid.
    -> Signal Bool     -- ^ Read ready.    
    -> ()
  )
axi_light_signature =
  input  "S_AXI_ACLK"    $ \s_axi_aclk    ->       
  input  "S_AXI_ARESETN" $ \s_axi_aresetn -> 
  input  "S_AXI_AWADDR"  $ \s_axi_awaddr  ->
  input  "S_AXI_AWPROT"  $ \s_axi_awprot  ->
  input  "S_AXI_AWVALID" $ \s_axi_awvalid -> 
  output "S_AXI_AWREADY" $ \s_axi_awready ->
  input  "S_AXI_WDATA"   $ \s_axi_wdata   ->
  input  "S_AXI_WSTRB"   $ \s_axi_wstrb   ->
  input  "S_AXI_WVALID"  $ \s_axi_wvalid  ->   
  output "S_AXI_WREADY"  $ \s_axi_wready  ->   
  output "S_AXI_BRESP"   $ \s_axi_bresp   ->     
  output "S_AXI_BVALID"  $ \s_axi_bvalid  ->   
  input  "S_AXI_BREADY"  $ \s_axi_bready  ->   
  input  "S_AXI_ARADDR"  $ \s_axi_araddr  ->
  input  "S_AXI_ARPROT"  $ \s_axi_arprot  ->
  input  "S_AXI_ARVALID" $ \s_axi_arvalid ->   
  output "S_AXI_ARREADY" $ \s_axi_arready ->
  output "S_AXI_RDATA"   $ \s_axi_rdata   ->     
  output "S_AXI_RRESP"   $ \s_axi_rresp   ->     
  output "S_AXI_RVALID"  $ \s_axi_rvalid  ->
  input  "S_AXI_RREADY"  $ \s_axi_rready  ->   
  ret $ axi_light
    s_axi_aclk s_axi_aresetn
    s_axi_awaddr s_axi_awprot s_axi_awvalid s_axi_awready s_axi_wdata s_axi_wstrb s_axi_wvalid s_axi_wready
    s_axi_bresp  s_axi_bvalid s_axi_bready
    s_axi_araddr s_axi_arprot s_axi_arvalid s_axi_arready s_axi_rdata
    s_axi_rresp  s_axi_rvalid s_axi_rready     
    
--------------------------------------------------------------------------------

axi_light
    s_axi_aclk s_axi_aresetn
    s_axi_awaddr s_axi_awprot s_axi_awvalid s_axi_awready s_axi_wdata s_axi_wstrb s_axi_wvalid s_axi_wready
    s_axi_bresp  s_axi_bvalid s_axi_bready
    s_axi_araddr s_axi_arprot s_axi_arvalid s_axi_arready s_axi_rdata
    s_axi_rresp  s_axi_rvalid s_axi_rready

  = do ----------------------------------------
       -- AXI Light signals.
       --
       awaddr  <- signal "axi_awaddr"  :: P (Signal (Bit 4))
       awready <- signal "axi_awready" :: P (Signal Bool)
       wready  <- signal "axi_wready"  :: P (Signal Bool)
       bresp   <- signal "axi_bresp"   :: P (Signal (Bit 2))
       bvalid  <- signal "axi_bvalid"  :: P (Signal Bool)
       araddr  <- signal "axi_araddr"  :: P (Signal (Bit 4))
       arready <- signal "axi_arready" :: P (Signal Bool)
       rdata   <- signal "axi_rdata"   :: P (Signal (Bit 32))
       rresp   <- signal "axi_rresp"   :: P (Signal (Bit 2))
       rvalid  <- signal "axi_rvalid"  :: P (Signal Bool)

       ----------------------------------------
       -- Application-specific design signals.
       --
       addr_lsb  <- constant "ADDR_LSB"          2 :: P (Constant Integer)
       addr_bits <- constant "OPT_MEM_ADDR_BITS" 1 :: P (Constant Integer)

       ----------------------------------------
       -- Signals for user logic registers.
       --
       reg_0     <- signal "slv_reg0"     :: P (Signal (Bit 32))
       reg_1     <- signal "slv_reg1"     :: P (Signal (Bit 32))
       reg_2     <- signal "slv_reg2"     :: P (Signal (Bit 32))
       reg_3     <- signal "slv_reg3"     :: P (Signal (Bit 32))
       reg_rdent <- signal "slv_reg_rden" :: P (Signal Bool)
       reg_wren  <- signal "slv_reg_wren" :: P (Signal Bool)
       reg_out   <- signal "reg_data_out" :: P (Signal (Bit 32))
       reg_index <- signal "byte_index"   :: P (Signal Integer)

       ----------------------------------------
       -- I/O Connections.
       --
       s_axi_awready <== awready
       s_axi_wready  <== wready
       s_axi_bresp   <== bresp
       s_axi_bvalid  <== bvalid
       s_axi_arready <== arready
       s_axi_rdata   <== rdata
       s_axi_rresp   <== rresp
       s_axi_rvalid  <== rvalid

       ----------------------------------------
       -- AXI_AWREADY generation.
       --
       process (s_axi_aclk .: []) $ do
         clk <- get s_axi_aclk
         rst <- get s_axi_aresetn
         when (risingEdge clk) $ do
           iff (isLow rst)
             (do awready <-- low)
             (do rdy <- get awready
                 awv <- get s_axi_awvalid
                 wv  <- get s_axi_wvalid
                 iff (isLow rdy `and` isHigh awv `and` isHigh wv)
                   (do awready <-- high)
                   (do awready <-- low))

       ----------------------------------------
       -- AXI_AWADDR latching.
       --
       process (s_axi_aclk .: []) $ do
         clk <- get s_axi_aclk
         rst <- get s_axi_aresetn
         when (risingEdge clk) $ do
           iff (isLow rst)
             (do setOthers awaddr (litE low))
             (do rdy <- get awready
                 awv <- get s_axi_awvalid
                 wv  <- get s_axi_wvalid
                 when (isLow rdy `and` isHigh awv `and` isHigh wv) $
                   awaddr <== s_axi_awaddr)

       ----------------------------------------
       -- AXI_AWREADY generation.
       --
       process (s_axi_aclk .: []) $ do
         clk <- get s_axi_aclk
         rst <- get s_axi_aresetn
         when (risingEdge clk) $ do
           iff (isLow rst)
             (do wready <-- low)
             (do rdy <- get awready
                 awv <- get s_axi_awvalid
                 wv  <- get s_axi_wvalid
                 iff (isLow rdy `and` isHigh awv `and` isHigh wv)
                   (wready <-- high)
                   (wready <-- low))

       ----------------------------------------
       -- Slave register logic.
       --
       process (s_axi_aclk .: []) $ do
         clk   <- get s_axi_aclk
         rst   <- get s_axi_aresetn
         when (risingEdge clk) $ do
           iff (isLow rst)
             (do setOthers reg_0 (litE low)
                 setOthers reg_1 (litE low)
                 setOthers reg_2 (litE low)
                 setOthers reg_3 (litE low))
             (do clsb  <- getConstant addr_lsb
                 cbits <- getConstant addr_bits
                 local <- getSlice awaddr (clsb + cbits) (clsb) :: P (Variable (Bit 2))
                 wren  <- get reg_wren
                 when (isHigh wren) $ do
                   local <- unsafeFreezeVariable local
                   switched local [
                       is 0 $ do
                         for (litE 3 :: HExp (Bit 2)) $ \i -> do
                           ix <- getIndex s_axi_wstrb i
                           return ()
                     ]
                     (do return ()))
         
       return ()

low    = False
high   = True
isLow  = (`eq` litE low)
isHigh = (`eq` litE high)
get    = unsafeFreezeSignal

--------------------------------------------------------------------------------

wrap :: P ()
wrap  = component "AXI" axi_light_signature >> return ()


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
--------------------------------------------------------------------------------
{-
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
  do local_reset         <- newSignal                                :: P (Signal Bit)
     local_address       <- integer (Just (0, 512 :: Integer))       :: P (Signal Integer)
     local_address_valid <- newSignal                                :: P (Signal Bit)

     mm_control_register           <- newSignal                      :: P (Signal Bit32)
     mm_data_register              <- newSignal                      :: P (Signal Bit32)
     servo_position_register_array <- newArray    (litE 4)           :: P (Array Word4 Bit8)
     low_endstop_register_array    <- othersArray (litE 4) min_pulse :: P (Array Word4 Bit32)
     high_endstop_register_array   <- othersArray (litE 4) max_pulse :: P (Array Word4 Bit32)

     combined      <- newSignal                                      :: P (Signal Bit2)
     write_enable  <- newSignal                                      :: P (Signal Bit)
     send_read     <- newSignal                                      :: P (Signal Bit)

     current_state <- newSignal                                      :: P (Signal Word4)
     next_state    <- newSignal                                      :: P (Signal Word4)

--------------------------------------------------------------------------------
-- initial setup.
     areset  <- unsafeFreezeSignal s_axi_aresetn
     local_reset <-- not areset
     awvalid <- unsafeFreezeSignal s_axi_awvalid
     arvalid <- unsafeFreezeSignal s_axi_arvalid
     combined    <-- (awvalid `catB` arvalid)

--------------------------------------------------------------------------------
-- state_machine_update.
     process (s_axi_aclk .: []) $
       do clk <- unsafeFreezeSignal s_axi_aclk
          rst <- unsafeFreezeSignal local_reset
          when ((event clk) `and` (clk `eq` true)) $
            iff (rst `eq` true)
              (current_state <-- reset)
              (current_state <== next_state)

--------------------------------------------------------------------------------
-- state_machine_decisions.
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
          s <- unsafeFreezeSignal current_state
          switched s [
            -- idle.
              is 0 $ do
                 next_state <-- idle
            -- reset.
            , is 1 $ do
                 next_state <-- idle
                 c <- unsafeFreezeSignal combined
                 switch c [
                     is 1 $ next_state <-- reading
                   , is 2 $ next_state <-- writing
                   ]
            -- read transaction in progress.
            , is 2 $ do 
                 next_state    <-- reading
                 s_axi_arready <== s_axi_arvalid
                 s_axi_rvalid  <-- litE 0
                 s_axi_rresp   <-- litE 0
                 send_read     <-- litE 1
                 rdy <- unsafeFreezeSignal s_axi_rready
                 when (rdy `eq` true) $
                   next_state <-- complete
            -- write transaction in progress.
            , is 3 $ do 
                 next_state    <-- writing
                 s_axi_awready <== s_axi_awvalid
                 s_axi_wready  <== s_axi_wvalid
                 s_axi_bresp   <-- litE 0
                 s_axi_bvalid  <-- litE 1
                 rdy <- unsafeFreezeSignal s_axi_bready
                 when (rdy `eq` true) $
                   next_state <-- complete
            -- complete.
            , is 4 $ do 
                 cbd <- unsafeFreezeSignal combined
                 switched cbd [
                     is 0 $ next_state <-- idle
                   ]
                   (next_state <-- complete)
            ]
            -- otherwise reset.
            (next_state <-- reset)

--------------------------------------------------------------------------------
-- send_data_to_AXI_RDATA.
     process (send_read .: local_address .:
              servo_position_register_array .: mm_control_register .: mm_data_register .:
              low_endstop_register_array .: high_endstop_register_array .: []) $
       do s_axi_rdata <-- others (0 :: Bit)
          vald <- unsafeFreezeSignal local_address_valid
          read <- unsafeFreezeSignal send_read
          when ((vald `eq` true) `and` (read `eq` true)) $
            do addr <- unsafeFreezeSignal local_address
               switched addr [
                   is 0 $ s_axi_rdata <== mm_control_register
                 , is 4 $ s_axi_rdata <== mm_data_register
                 , 128 `to` 252 $ do
                     let ix = cast (W4 . fromInteger) $ (addr `sub` 128) `div` 4
                     serv <- getArray ix servo_position_register_array
                     s_axi_rdata <-- padB8 serv
                 , 256 `to` 380 $ do
                     let ix = cast (W4 . fromInteger) $ (addr `sub` 256) `div` 4
                     low <- getArray ix low_endstop_register_array
                     s_axi_rdata <-- low
                 , 384 `to` 508 $ do
                     let ix = cast (W4 . fromInteger) $ (addr `sub` 384) `div` 4
                     high <- getArray ix high_endstop_register_array
                     s_axi_rdata <-- high
                 ]
                 (null)

--------------------------------------------------------------------------------
-- local_address_capture_register
     process (s_axi_aclk .: []) $
       do clk <- unsafeFreezeSignal s_axi_aclk
          rst <- unsafeFreezeSignal local_reset
          when ((event clk) `and` (clk `eq` true)) $
            iff (rst `eq` true)
              (local_address <-- litE 0)
              (do addrv <- unsafeFreezeSignal local_address_valid
                  when (addrv `eq` true) $ do
                    comb <- unsafeFreezeSignal combined
                    switched comb [
                        is 2 $ return ()
                      , is 1 $ return ()
                      ]
                      (null)
              )
          return ()
  where
     reset, idle, reading, writing, complete :: HExp Word4
     reset    = litE 0
     idle     = litE 1
     reading  = litE 2
     writing  = litE 3
     complete = litE 4

     max_pulse, min_pulse :: HExp Bit32
     max_pulse = litE 2000000
     min_pulse = litE 1000000

padB8 :: HExp Bit8 -> HExp Bit32
padB8 = catB16 (litE 0) . catB8 (litE 0)
-}
--------------------------------------------------------------------------------
{-
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

-}
{-
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
-}
