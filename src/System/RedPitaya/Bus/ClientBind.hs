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
{-# LANGUAGE NoImplicitPrelude    #-}


module System.RedPitaya.Bus.ClientBind 

where
import CLaSH.Prelude
import System.RedPitaya.Fpga
import System.RedPitaya.Bus.Tools


--------------
writeCoreNice :: forall n rp . (  KnownNat n, FpgaSetGet rp) 
              => SNat n -> Offset -> Vec (n * 32) Bit -> rp ()
writeCoreNice n offset d = fpgaSetArray offset (u2reg <$> vec2split n d)

writeCoreV :: ((n * 32) ~ (m + k), FpgaSetGet rp, KnownNat k, KnownNat n) 
           => SNat n -> Offset -> Vec m Bit -> rp ()
writeCoreV n offset d = writeCoreNice n offset $ extendSize n d

writeCore :: ((n * 32) ~ (m + k), BitSize a ~ m, BitPack a,FpgaSetGet rp, 
             KnownNat k, KnownNat n, KnownNat m) 
          => SNat n -> Offset -> a -> rp ()
writeCore n o d = writeCoreV n o (bitCoerce d)

--------------
readCoreNice :: forall n rp . (  KnownNat n, FpgaSetGet rp) 
             => SNat n -> Offset -> rp (Vec (n * 32) Bit)
readCoreNice n offset = do
        regList <- fpgaGetArray offset ( fromInteger (snatToInteger n))
        let r =  fromListI regList :: Vec n Registry
        let b = fmap bitCoerce $ fmap reg2u r :: Vec n (Vec 32 Bit)
        return $ concat b

readCoreV :: ((n * 32) ~ (m + k), FpgaSetGet rp, KnownNat n, KnownNat m) 
          => SNat n -> Offset -> rp (Vec m Bit)
readCoreV n offset =  reduceSize n <$> readCoreNice n offset

readCore :: ((n * 32) ~ (m + k),BitSize a ~ m, BitPack a, FpgaSetGet rp, KnownNat n, KnownNat m) 
         => SNat n -> Offset -> rp a
readCore n o = bitCoerce <$> readCoreV n o

-------------
callCore :: ((ni * 32) ~ (mi + ki), KnownNat ki, KnownNat ni, KnownNat mi,
             BitSize a ~ mi, BitPack a,
             (no * 32) ~ (mo + ko), KnownNat no, KnownNat mo, 
             BitSize b ~ mo, BitPack b, FpgaSetGet rp)
         => (SNat ni,SNat no) ->  Offset 
         ->  Offset -> a ->  rp b
callCore (ni,no) oi oo d = writeCore ni oi d >> readCore no oo


