{-|
Copyright  :  Luka Rahne
License    :  LGPL-3 (see the file LICENSE)
Maintainer :  Luka Rahne <luka.rahne@gmail.com>
-}

{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE MagicHash            #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE TypeOperators        #-}


module System.RedPitaya.Bus.ClientBind 
(
   writeCore,
   readCore ,
   callCore
)
where
import CLaSH.Prelude
import qualified Prelude as P
import System.RedPitaya.Bus.CoreBind
import System.RedPitaya.Fpga

writeCore :: forall rp a a1 a2 a3 a4 .
             ( BusBuildC a a1 a2 a3 a4, KnownNat a4
             , FpgaSetGet rp) 
           => Page -> Offset 
           -> (SNat a2 , SNat a3) 
           -> a -> rp ()
writeCore page off (snatA,snatC) inData = sendData where
    splitp = snat :: SNat (a2 * 32)
    (vL,vR) = splitAt splitp $ bv2v $ pack inData
    vlM = unconcatI vL :: Vec a2 (Vec 32 Bit)
    vRfill = def :: Vec a4 Bit
    lstV = vR ++ vRfill :: Vec 32 Bit
    arr = v2r <$> toList vlM :: [Registry]
    v2i x = bitCoerce x :: Unsigned 32
    v2r = fromInteger . toInteger . v2i :: Vec 32 Bit -> Registry
    sendData =  writeFpgaArray page off ( arr P.++ [v2r lstV])


readCore :: forall rp a a1 a2 a3 a4 .
             ( BusBuildC a a1 a2 a3 a4
             , FpgaSetGet rp) 
           => Page -> Offset 
           -> (SNat a2 , SNat a3) 
           -> rp a
readCore page off (snatA,snatC) = r where
  snatAint = fromInteger ( snatToInteger snatA) :: Int
  snatCint = fromInteger ( snatToInteger snatC) :: Int
  r = fdata <$> readFpgaArray page off (snatAint+1)  where
    fdata rsp = unpack (arrB ++# lastB) where
      arrV = fromInteger . toInteger  <$> fromListI rsp :: Vec a2 (Unsigned 32)
      arrB  = pack arrV :: BitVector (a2 * 32)
      lastVl = fromInteger $ toInteger $ P.last rsp  :: Unsigned 32
      lastB = pack $ truncateB $ lastVl `shiftR` (32 - snatCint) :: BitVector a3



callCore ::  ( BusBuildC a a1 a2 a3 a4
             , BusBuildC b b1 b2 b3 b4
             , KnownNat a4
             , FpgaSetGet rp) 
           => (SNat a2, SNat a3, SNat b2, SNat b3) 
           -> Page -> Offset
           -> Page -> Offset
           -> a -> rp b
callCore (sinA,sinC,soutA,soutC) pw ow pr or din =
   writeCore pw ow (sinA,sinC) din >> readCore pw or (soutA,soutC)

fromListUnsafeU :: UNat n -> [a] -> Vec n a
fromListUnsafeU UZero  _  = Nil
fromListUnsafeU (USucc s) (x:xs) = x :> fromListUnsafeU s xs

fromListUnsafe :: SNat n -> [a] -> Vec n a
fromListUnsafe n = fromListUnsafeU (toUNat n)

fromListI :: (Default a ,KnownNat n) =>  [a] -> Vec n a
fromListI xs = fromListUnsafe snat $ xs P.++  P.repeat def
