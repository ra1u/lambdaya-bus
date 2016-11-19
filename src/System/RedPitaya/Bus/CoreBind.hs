{-|
Copyright  :  Luka Rahne
License    :  LGPL-3 (see the file LICENSE)
Maintainer :  Luka Rahne <luka.rahne@gmail.com>
-}

{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE NoImplicitPrelude    #-}

module System.RedPitaya.Bus.CoreBind
where


import CLaSH.Prelude hiding (v)
import System.RedPitaya.Bus.Tools
import System.RedPitaya.Bus.RedPitayaSimple


busWriteNice :: (Enum addr, KnownNat n )
                      => addr
                      -> Vec (n*32) Bit
                      -> FullDataOut
busWriteNice a v = ( bitCoerce <$> unconcatI v ) !! a

busWriteV :: ((n * 32) ~ (m + k), Enum a , KnownNat k, KnownNat n) 
           => SNat n -> a -> Vec m Bit -> FullDataOut
busWriteV n a v = busWriteNice a $ extendSize n v

busWrite :: ((n * 32) ~ (m + k), BitSize d ~ m, BitPack d, Enum a, KnownNat k, KnownNat n, KnownNat m) 
          => SNat n
          -> Signal (Maybe a) 
          -> Signal (Maybe d)
          -> Signal (Maybe FullDataOut)
busWrite sn = liftA2 $ liftA2 $ \m' v' -> busWriteV sn m' ( bitCoerce v')

--------------------------------------------
busReadNice :: (Enum a, KnownNat n )
                      => Maybe a 
                      -> FullDataIn
                      -> Vec (n*32) Bit
                      -> Vec (n*32) Bit
busReadNice a d v = concat $ replaceM a (bitCoerce d) (unconcatI v)


busRead :: forall n m k d a . 
                 ((n * 32) ~ (m + k), BitSize d ~ m, 
                 BitPack d, Enum a, KnownNat k, KnownNat n, KnownNat m) 
          => SNat n
          -> Signal (Maybe a) 
          -> Signal (FullDataIn)
          -> Signal d
busRead n a d =  fmap bitCoerce v where
    v :: Signal (Vec m Bit)
    v = register def $ reduceSize n <$> (busReadNice <$> a <*> d <*> (extendSize n <$> v))



coreBind :: ((n1 * 32) ~ (m1 + k1), BitSize d1 ~ m1, BitPack d1, 
                Enum a1, KnownNat k1, KnownNat n1, KnownNat m1,
             (n2 * 32) ~ (m2 + k2), BitSize d2 ~ m2, BitPack d2, 
                Enum a2, KnownNat k2, KnownNat n2, KnownNat m2)
         => (SNat n1,SNat n2)
         -> Signal (Maybe a1)
         -> Signal (Maybe a2)
         -> (Signal d1 -> Signal (Maybe d2))
         -> Signal FullDataIn
         -> Signal (Maybe FullDataOut)
coreBind (n1,n2) aW aR core din = busWrite n2 aW ( core ( busRead n1 aR din ) )


----------------------------------------------------------------

busBind :: ((n1 * 32) ~ (m1 + k1), BitSize d1 ~ m1, BitPack d1, 
                KnownNat k1, KnownNat n1, KnownNat m1,
             (n2 * 32) ~ (m2 + k2), BitSize d2 ~ m2, BitPack d2, 
                KnownNat k2, KnownNat n2, KnownNat m2,
                Enum a1)
         => (SNat n1,SNat n2)
         -> (Signal d1 -> Signal (Maybe d2))
         -> Signal (Maybe (a1,ReadWrite,FullDataIn))
         -> Signal (Maybe FullDataOut)
busBind nx core din = coreBind nx aR aW core fullIn where
              (aR,aW,fullIn) = unbundle (fmap splitBus din)
                 where
                  splitBus Nothing = (Nothing,Nothing,0)
                  splitBus (Just (ar,Read,_)) = (Just ar,Nothing,0)
                  splitBus (Just (aw,Write,d)) = (Nothing,Just aw,d)
