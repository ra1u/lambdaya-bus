{-|
Copyright  :  Luka Rahne
License    :  LGPL-3 (see the file LICENSE)
Maintainer :  Luka Rahne <luka.rahne@gmail.com>
-}

{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE MagicHash            #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE FlexibleContexts     #-}

module System.RedPitaya.Bus.CoreBind (
   BuildThC(..),
   BuildThModC(..),
   BusBuildC(..),
   busBuild,
   bTQ,
)
where


import CLaSH.Prelude
import CLaSH.Sized.Vector ( concatBitVector# )
import System.RedPitaya.Bus.RedPitayaSimple
import Language.Haskell.TH
import qualified Prelude

-- | constraint for defining type being `BitPack` 
type BuildThC a b = ( KnownNat b, KnownNat (BitSize a), BitPack a, (BitSize a) ~ b )

--  | constraint for  a = (b * 32) + c and c <= 32
type BuildThModC a b c d = ( KnownNat a, KnownNat b, KnownNat c, KnownNat (b * 32)
                  , ((b * 32) + c) ~ a, (c + d) ~ 32)

-- | constraint `BuildThModC` and `BuildThC`
type BusBuildC a a1 a2 a3 a4 = ( BuildThC a a1 , BuildThModC a1 a2 a3 a4 )

---------------------------
------- Bus -> Core
---------------------------

-- Produce vector of data from values consumed from bus
coreBusReadVec :: (Num adr,Eq adr,KnownNat n) 
               => Signal (Maybe (adr,FullDataIn)) -- addr is single step ,data
               -> Signal (Vec n FullDataIn) -- data
coreBusReadVec = mealy mf def where
    mf s Nothing = (s,s)
    mf s (Just (inAdd,inData)) = (o,o) where
      o = zipWith zf s (iterateI (+1) 0)
      zf curData addrN = if (inAdd == addrN) then inData else curData


-- Single value bus consumer same input/output size
coreBusReadSingleGen :: forall a . (KnownNat a) 
                 => Signal ( Maybe (BitVector a) )
                 -> Signal ( BitVector a )
coreBusReadSingleGen = mealy mf def where
    mf s Nothing  = (s,s)
    mf s (Just d) = (d,d)

-- Single value bus consumer
coreBusReadSingle :: forall a a' . (KnownNat a,(a + a') ~ 32) 
                 => Signal ( Maybe FullDataIn )
                 -> Signal ( BitVector a )
coreBusReadSingle s = coreBusReadSingleGen r where
                       r =  (fmap (v2bv . takey . bv2v . pack) <$> s)
                       takey = takeI :: Vec (a+a') Bit -> Vec a Bit

-- reads from bus genericaly, for now it can read at lest 33 bits or more.
-- if user needs to read width with less data, than it can use coreBusReadSingle
coreBusReadSimple :: forall a c c' adr . 
                       (KnownNat a,KnownNat c,Num adr,Eq adr,(c + c') ~ 32)
                  => (SNat a,SNat c) 
                  -> Signal ( Maybe (adr,FullDataIn) )
                  -> Signal ( BitVector ((a * 32) + c))
coreBusReadSimple (sNatA,sNatC) sig = r where
    -- join left of size a*32 and right of size c 
    -- left
    lvec = coreBusReadVec sig  :: Signal (Vec a FullDataIn)
    lbvec = fmap concatBitVector# ( fmap (fmap pack ) lvec ) :: Signal ( BitVector (a * 32))
    -- right
    rval = coreBusReadSingle sigR :: Signal ( BitVector c)
        where 
           sigR = fmap (>>= f) sig :: Signal ( Maybe FullDataIn )
           addR = fromInteger $ snatToInteger $ sNatA 
           f (add,d) 
                | addR == add  = Just d
                | otherwise  = Nothing
    -- merge
    r = (++#) <$> lbvec <*> rval :: Signal ( BitVector ((a * 32) + c))

-- Ready any width (short or wide)
coreBusReadGeneric :: forall a a1 a2 a3 a4 adr. 
                      ( BusBuildC a a1 a2 a3 a4 , Eq adr, Num adr)
                   => ( SNat a2 , SNat a3 )
                   -> Signal ( Maybe (adr,ReadWrite,FullDataIn) )
                   -> Signal a
coreBusReadGeneric (sinA,sinC) busIn = unpack <$> r where
      fInWide (Just (addr,Write,d)) = Just (addr,d)
      fInWide _ = Nothing
      -- 
      fInShort (Just (addr,Write,d)) = Just d
      fInShort _ = Nothing
      r = -- input is wide use coreBusReadSimple
       if not (snatToInteger sinA == 0 )  then 
        coreBusReadSimple (sinA,sinC) $ fInWide <$> busIn
       else -- use coreBusReadSingle (compiler does not know that sinA == 0)
        let sInShort = coreBusReadSingle (fInShort <$> busIn) :: Signal (BitVector a3) 
        in (def ++#) <$> sInShort :: Signal (BitVector a1)  

---------------------------
------- Core -> Bus
---------------------------
--  write vector on bus
coreBusWriteVec   :: (Eq a, Num a, KnownNat n )
                      => a 
                      -> Vec n FullDataOut
                      -> Maybe FullDataOut
coreBusWriteVec addr0 d = fold (<|>) v where 
             v = Nothing :> zipWith zf d (iterateI (+1) 0) 
             zf x y = if addr0 == y then Just x else Nothing

--  write value on bus
coreBusWriteSingle   :: (KnownNat n, KnownNat n', (n + n') ~ 32)
                      => Bool -- same
                      -> BitVector n 
                      -> Maybe FullDataOut
coreBusWriteSingle same d
     | same = Just $ unpack (d ++# def)
     | otherwise = Nothing 

-- write for Bitsize >= 33
coreBusWriteSimple :: forall ad a0 a c c' . 
                     (Num ad, Eq ad, BuildThModC a0 a c c',KnownNat c')
                  => (SNat a,SNat c)
                  -> Maybe ad
                  -> BitVector ((a * 32) + c)
                  -> Maybe FullDataOut
coreBusWriteSimple _ Nothing _ = Nothing
coreBusWriteSimple (snatA,snatC) (Just addr) din = left <|> right where
    vin =  (unconcatI $ takei $ bv2v din) :: Vec a (Vec 32 Bit)
    left = coreBusWriteVec addr $ unpack . v2bv <$> vin 
    rD = v2bv $ drop (mulSNat snatA d32) $ bv2v din :: BitVector c
    right = coreBusWriteSingle (addr == (fromInteger $ snatToInteger $ lengthS vin)) rD
    takei  = takeI ::  (Vec ((a * 32) + c) Bit) -> Vec (a * 32) Bit

-- write wide and narrow values on bus
coreBusWriteGeneric :: forall a a1 a2 a3 a4 adr. 
                      ( BusBuildC a a1 a2 a3 a4 , KnownNat a4, Eq adr, Num adr)
                   => ( SNat a2 , SNat a3 )
                   -> Signal ( Maybe (adr,a) )
                   -> Signal ( Maybe FullDataOut )
coreBusWriteGeneric (soutA,soutC) sigIn = fmap ( >>= readBus ) sigIn where
      shrAdd (addr,d) = (aSh,pack d)  :: (adr,BitVector a1) where
        aSh = addr
      rV (addr,d) = 
        if not (snatToInteger soutA == 0 ) then 
         coreBusWriteSimple (soutA,soutC) (Just addr) d
        else
         coreBusWriteSingle (addr == 0) (v2bv dFake) where
          dFake = dropI (bv2v d) :: (Vec a3 Bit)
      readBus = rV . shrAdd 

-----------------------------------
----------convert core to bus
-----------------------------------

busBuild :: forall adr a a1 a2 a3 a4 b b1 b2 b3 b4. 
                                 ( BusBuildC a a1 a2 a3 a4
                                 , BusBuildC b b1 b2 b3 b4
                                 , KnownNat b4) 
           => (SNat a2 , SNat a3, SNat b2, SNat b3) 
           -> ( Signal a -> Signal  b ) 
           -> Signal BusIn
           -> Signal BusOut
busBuild types core busInput = sigOut
  where   
    (sinA,sinC,soutA,soutC) = types 
    busIn = fmap f <$> busInput where
       f (addr,dir,datum) = (addr `shiftR` 2 ,dir,datum)
    -- bus to core
    sigInWide = coreBusReadGeneric (sinA,sinC) busIn  
 
    -- core output back to bus
    sigOutWide = core sigInWide :: Signal b
    sigReadData :: Signal (Maybe (FullAddress,b))
    sigReadData = f <$> busIn <*> sigOutWide where
      f Nothing _ = Nothing
      f (Just (_,Write,_)) _ = Nothing
      f (Just (fullAddr,Read,_)) d = Just (fullAddr,d)
    
    sigOutBus = coreBusWriteGeneric (soutA,soutC) sigReadData :: Signal (Maybe FullDataOut)

    sigOut = (<|>) <$> sigOutBus <*> signal (Just 0) -- allways valid


busE :: forall a a1 b b1.(BuildThC a a1, BuildThC b b1)
       => (Signal a -> Signal b) 
       -> Exp
busE f =  TupE [snatT ap1,snatT ap2,snatT bp1,snatT bp2] where
            snatT n = SigE (VarE 'snat ) 
                           (AppT (ConT ''SNat) 
                           (LitT (NumTyLit n)))
            fr n = (ra,rb) where
                (a,b) = quotRem n 32
                (ra,rb) | b == 0 = (a-1,32)
                        | otherwise = (a,b)
            (ap1,ap2) = fr (snatToInteger (snat :: SNat a1))
            (bp1,bp2) = fr (snatToInteger (snat :: SNat b1))


-- | Template that helps deducing first argument of function 'busBuild'
bTQ :: forall a a1 b b1.(BuildThC a a1, BuildThC b b1)
       => (Signal a -> Signal b) 
       -> ExpQ
bTQ = return . busE
