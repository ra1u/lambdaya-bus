{-|
Copyright  :  Luka Rahne
License    :  LGPL-3 (see the file LICENSE)
Maintainer :  Luka Rahne <luka.rahne@gmail.com>
-}

{-# LANGUAGE DataKinds             #-}


module System.RedPitaya.Bus.RedPitayaSimple
(
    RpBusAddress,
    FullAddress,
    FullDataIn,
    FullDataOut,
    WriteByteSel,
    RpBusSysIn(..),
    RpBusSysOut(..),
    ReadWrite(..),
    BusRwIn,
    BusWOut,
    defTopRedPitayaSimple,
    rpSimpleBind,
    addrLow
)
where
import CLaSH.Prelude
import qualified Prelude as P

-- | this definition should be used to define bus that can be directly build
-- with fpga core defined in redpitaya branch clash https://github.com/ra1u/RedPitaya/tree/clash
defTopRedPitayaSimple = (defTop
    { t_name     = "red_pitaya_clash_bus"
    , t_inputs   = ["add_i","data_i","strobe_i","we_i","re_i"]
    , t_outputs  = ["data_o","ack_o","err_o"]
    })


type RpBusAddress = Unsigned 32
type FullAddress = Unsigned 20
type FullDataIn  = Unsigned 32
type FullDataOut = Unsigned 32

type WriteByteSel = Unsigned 4

data RpBusSysIn = RpBusSysIn {
    addrRpBus :: RpBusAddress,
    dataInRpBus :: FullDataIn,
    strobeWidthRpBus :: WriteByteSel,
    writeEnableRpBus :: Bool,
    readEnableRpBus :: Bool
} deriving(Show)

data RpBusSysOut = RpBusSysOut {
    dataOut :: FullDataOut,
    ack :: Bool,
    err :: Bool
} deriving(Show)

data ReadWrite = Read | Write

type BusRwIn =  Maybe (FullAddress,ReadWrite,FullDataIn)
type BusWOut = Maybe FullDataOut
 

-- | provide redPitayaSimple interface ovet simplified bus 
-- where redPitayaSimple is bus as defined <https://github.com/ra1u/RedPitaya/blob/clash/fpga/rtl/red_pitaya_top.v>
rpSimpleBind :: (Signal BusRwIn -> Signal BusWOut) 
                -> Signal RpBusSysIn 
                -> Signal RpBusSysOut
rpSimpleBind f sig = postProc <$> bundle (fin,fout)
    where 
     fin = fmap preProc sig
     fout = f fin
     preProc din
        | we == re = Nothing
        | otherwise  =  Just  (truncateB (addrRpBus din),getRw ,dataInRpBus din) :: BusRwIn
             where
               we = writeEnableRpBus din
               re = readEnableRpBus din
               getRw 
                   | we = Write
                   | otherwise = Read
     postProc (Just inp,Just out) = RpBusSysOut out True False
     postProc _ = RpBusSysOut 0 False False


-- | remove away MSB part of address with page info away 
addrLow :: Signal BusRwIn -> Signal BusRwIn
addrLow sig =  fmap ( fmap f )  sig where
    f (addr,m,din) = (addr .&.0xFFFFF,m,din) 



