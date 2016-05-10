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
       awaddr  <- signal "axi_awaddr"  :: P (Signal (Bits 4))
       awready <- signal "axi_awready" :: P (Signal Bit)
       wready  <- signal "axi_wready"  :: P (Signal Bit)
       bresp   <- signal "axi_bresp"   :: P (Signal (Bits 2))
       bvalid  <- signal "axi_bvalid"  :: P (Signal Bit)
       araddr  <- signal "axi_araddr"  :: P (Signal (Bits 4))
       arready <- signal "axi_arready" :: P (Signal Bit)
       rdata   <- signal "axi_rdata"   :: P (Signal (Bits 32))
       rresp   <- signal "axi_rresp"   :: P (Signal (Bits 2))
       rvalid  <- signal "axi_rvalid"  :: P (Signal Bit)

       ----------------------------------------
       -- Application-specific design signals.
       --
       addr_lsb  <- constant "ADDR_LSB"          2 :: P (Constant Integer)
       addr_bits <- constant "OPT_MEM_ADDR_BITS" 1 :: P (Constant Integer)

       ----------------------------------------
       -- Signals for user logic registers.
       --
       reg_0     <- signal "slv_reg0"     :: P (Signal (Bits 32))
       reg_1     <- signal "slv_reg1"     :: P (Signal (Bits 32))
       reg_2     <- signal "slv_reg2"     :: P (Signal (Bits 32))
       reg_3     <- signal "slv_reg3"     :: P (Signal (Bits 32))
       reg_rden  <- signal "slv_reg_rden" :: P (Signal Bit)
       reg_wren  <- signal "slv_reg_wren" :: P (Signal Bit)
       reg_out   <- signal "reg_data_out" :: P (Signal (Bits 32))
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
             (do awaddr <:- low)
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
             (do reg_0 <:- low
                 reg_1 <:- low
                 reg_2 <:- low
                 reg_3 <:- low)
             (do clsb  <- getConstant addr_lsb
                 cbits <- getConstant addr_bits
                 -- *** todo: lift these constants to a range constraint.
                 slice <- getSlice awaddr (R :: Range 1 (1 + 2))
                 wren  <- get reg_wren
                 when (isHigh wren) $ do
                   let iter = litE 3 :: HExp Integer
                   i <- integer slice
                   switched i [
                       is 0 $ for iter $ \i -> do
                          strb <- getIndex s_axi_wstrb i
                          when (isHigh strb) $ do
                            copySliceDynamic
                              reg_0       (i*8+7, i*8)
                              s_axi_wdata (i*8+7, i*8)
                     , is 1 $ do
                          strb <- getIndex s_axi_wstrb i
                          when (isHigh strb) $ do
                            copySliceDynamic
                              reg_1       (i*8+7, i*8)
                              s_axi_wdata (i*8+7, i*8)
                     , is 2 $ do
                          strb <- getIndex s_axi_wstrb i
                          when (isHigh strb) $ do
                            copySliceDynamic
                              reg_2       (i*8+7, i*8)
                              s_axi_wdata (i*8+7, i*8)
                     , is 3 $ do
                          strb <- getIndex s_axi_wstrb i
                          when (isHigh strb) $ do
                            copySliceDynamic
                              reg_3       (i*8+7, i*8)
                              s_axi_wdata (i*8+7, i*8)
                     ]
                     (do reg_0 <== reg_0
                         reg_1 <== reg_1
                         reg_2 <== reg_2
                         reg_3 <== reg_3))

       ----------------------------------------
       -- Write response generation.
       --
       process (s_axi_aclk .: []) $ do
         clk   <- get s_axi_aclk
         rst   <- get s_axi_aresetn
         when (risingEdge clk) $ do
           iff (isLow rst)
             (do bvalid <-- low
                 bresp  <:- low)
                 --bresp  <-- bitFromInteger 0)
             (do rdy  <- get awready
                 awv  <- get s_axi_awvalid
                 wrdy <- get awready
                 wv   <- get s_axi_wvalid
                 bv   <- get bvalid
                 brdy <- get s_axi_bready
                 ifE
                   ( isHigh rdy  `and` isHigh awv `and`
                     isHigh wrdy `and` isHigh wv  `and`
                     isLow  bv
                   , do bvalid <-- high
                        bresp  <:- low)
                   ( isHigh brdy `and` isHigh bv
                    ,   bvalid <-- low))

       ----------------------------------------
       -- AXI ARREADY generation.
       --
       process (s_axi_aclk .: []) $ do
         clk   <- get s_axi_aclk
         rst   <- get s_axi_aresetn
         when (risingEdge clk) $ do
           iff (isLow rst)
             (do arready <-- low
                 araddr  <:- high)
             (do ready <- get arready
                 valid <- get s_axi_arvalid
                 iff (isLow ready `and` isHigh valid)
                   (araddr  <== s_axi_araddr)
                   (arready <-- low))

       ----------------------------------------
       -- AXI ARVALID generation.
       --
       process (s_axi_aclk .: []) $ do
         clk   <- get s_axi_aclk
         rst   <- get s_axi_aresetn
         when (risingEdge clk) $ do
           iff (isLow rst)
             (do rvalid <-- low
                 rresp  <:- low)
             (do rdy  <- get arready
                 av   <- get s_axi_arvalid
                 rrdy <- get s_axi_rready
                 rv   <- get rvalid
                 ifE
                   ( isHigh rdy `and` isHigh av `and` isLow rv
                   , do rvalid <-- high
                        rresp  <:- low)
                   ( isHigh rv  `and` isHigh rrdy
                   , do rvalid <-- low))

       ----------------------------------------
       -- ...
       --
       rrdy <- get arready
       arv  <- get s_axi_arvalid
       rv   <- get rvalid
       reg_rden <=- (rrdy `and` arv `and` not rv)
       process (s_axi_aclk .: []) $ do
         clk   <- get s_axi_aclk
         when (risingEdge clk) $ do
           -- *** todo: lift these constants to a range constraint.
           slice <- getSlice araddr (R :: Range 1 (1 + 2))
           i     <- integer slice                 
           switch i [
               is 0 $ reg_out <== reg_0
             , is 1 $ reg_out <== reg_1
             , is 2 $ reg_out <== reg_2
             , is 3 $ reg_out <== reg_3
             ]

       ----------------------------------------
       -- Output register or memory read data.
       --
       process (s_axi_aclk .: []) $ do
         clk   <- get s_axi_aclk
         rst   <- get s_axi_aresetn
         when (risingEdge clk) $ do
           iff (isLow rst)
             (do rdata <:- low)
             (do rden <- get reg_rden
                 when (isLow rden) $
                   rdata <== reg_out)

low    = False
high   = True
isLow  = (`eq` litE low)
isHigh = (`eq` litE high)
get    = unsafeFreezeSignal

(<:-) :: Signal (Bits n) -> Bool -> P ()
(<:-) s b = setOthers s (litE b)

--------------------------------------------------------------------------------

wrap :: P ()
wrap  = component "AXI" axi_light_signature >> return ()


test :: IO ()
test = putStrLn $ compile $ wrap

--------------------------------------------------------------------------------
