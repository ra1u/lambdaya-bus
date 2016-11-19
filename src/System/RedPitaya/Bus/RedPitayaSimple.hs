{-|
Copyright  :  Luka Rahne
License    :  LGPL-3 (see the file LICENSE)
Maintainer :  Luka Rahne <luka.rahne@gmail.com>
-}

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

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
    BusIn,
    BusOut,
    defTopRedPitayaSimple,
    rpSimpleBind,
    addrLow,
    mapAddr
)
where
import CLaSH.Prelude
import GHC.Generics (Generic)
import Control.DeepSeq

-- | this definition should be used to define bus that can be directly build
-- with fpga core defined in redpitaya branch clash https://github.com/ra1u/RedPitaya/tree/clash
defTopRedPitayaSimple :: TopEntity
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

data ReadWrite = Read | Write deriving (Show,Generic, NFData) 

type BusIn =  Maybe (FullAddress,ReadWrite,FullDataIn)
type BusOut = Maybe FullDataOut


-- | provide redPitayaSimple interface ovet simplified bus 
-- where redPitayaSimple is bus as defined <https://github.com/ra1u/RedPitaya/blob/clash/fpga/rtl/red_pitaya_top.v>
rpSimpleBind :: (Signal BusIn -> Signal BusOut) 
                -> Signal RpBusSysIn 
                -> Signal RpBusSysOut
rpSimpleBind f sig = postProc <$> bundle (fin,fout)
    where 
     fin = fmap preProc sig
     fout = f fin
     preProc din
        | we == re = Nothing
        | otherwise  =  Just  (truncateB (addrRpBus din),getRw ,dataInRpBus din) :: BusIn
             where
               we = writeEnableRpBus din
               re = readEnableRpBus din
               getRw 
                   | we = Write
                   | otherwise = Read
     postProc (Just _ ,Just out) = RpBusSysOut out True False
     postProc _ = RpBusSysOut 0 False False


{-# INLINE mapAddr #-}
mapAddr :: (a->b) -> Maybe (a,ReadWrite,FullDataIn) -> Maybe (b,ReadWrite,FullDataIn)
mapAddr = fmap . fst' where fst' f (a,b,c) = (f a,b,c)
 

{-# INLINE addrLow #-}
-- | remove away MSB part of address with page info away 
addrLow :: Signal BusIn -> Signal BusIn
addrLow =  fmap (mapAddr (.&.0xFFFFF))


