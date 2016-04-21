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
type BuildThC a = (BitPack a, KnownNat (BitSize a))

--  | constraint for  a = (b * 32) + c and c <= 32
type BuildThModC a1 a2 a3 a4 = ( KnownNat a1, KnownNat a2, KnownNat a3, KnownNat a4, KnownNat (a2 * 32)
                  , ((a2 * 32) + a3) ~ a1, (a3 + a4) ~ 32)

-- | constraint `BuildThModC` and `BuildThC`
type BusBuildC a a1 a2 a3 a4 = (BitSize a  ~ a1 ,BuildThC a , BuildThModC a1 a2 a3 a4 )

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
coreBusReadSimple :: forall a1 a2 a3 adr . 
                       (KnownNat a1, KnownNat a2,KnownNat a3,Num adr,Eq adr,(a2 + a3) ~ 32)
                  => (SNat a1,SNat a2,SNat a3) 
                  -> Signal ( Maybe (adr,FullDataIn) )
                  -> Signal ( BitVector ((a1 * 32) + a2))
coreBusReadSimple (sa1,sa2,sa3) sig = r where
    -- join left of size a*32 and right of size c 
    -- left
    lvec = coreBusReadVec sig  :: Signal (Vec a1 FullDataIn)
    lbvec = fmap concatBitVector# ( fmap (fmap pack ) lvec ) :: Signal ( BitVector (a1 * 32))
    -- right
    rval = coreBusReadSingle sigR :: Signal ( BitVector a2)
        where 
           sigR = fmap (>>= f) sig :: Signal ( Maybe FullDataIn )
           addR = fromInteger $ snatToInteger $ sa1 
           f (add,d) 
                | addR == add  = Just d
                | otherwise  = Nothing
    -- merge
    r = (++#) <$> lbvec <*> rval :: Signal ( BitVector ((a1 * 32) + a2))


type SNat4 a b c d = ( SNat a, SNat b, SNat c, SNat d)

snatTail (a,b,c,d) = (b,c,d)

t41 (a,b,c,d) = a
t42 (a,b,c,d) = b
t43 (a,b,c,d) = c
t44 (a,b,c,d) = d


-- Ready any width (short or wide)
coreBusReadGeneric :: forall a a1 a2 a3 a4 adr. 
                      ( BusBuildC a a1 a2 a3 a4 , Eq adr, Num adr)
                   => ( SNat4 a1 a2  a3 a4)
                   -> Signal ( Maybe (adr,ReadWrite,FullDataIn) )
                   -> Signal a
coreBusReadGeneric snatT busIn = unpack <$> r where
      fInWide (Just (addr,Write,d)) = Just (addr,d)
      fInWide _ = Nothing
      -- 
      fInShort (Just (addr,Write,d)) = Just d
      fInShort _ = Nothing
      r = -- input is wide use coreBusReadSimple
       if not (snatToInteger (t42 snatT) == 0 )  then 
        coreBusReadSimple (snatTail snatT) $ fInWide <$> busIn
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
coreBusWriteSimple :: forall add a1 a2 a3 a4 . 
                     (Num add, Eq add, BuildThModC a1 a2 a3 a4)
                  => SNat4 a1 a2 a3 a4
                  -> Maybe add
                  -> BitVector a1
                  -> Maybe FullDataOut
coreBusWriteSimple _ Nothing _ = Nothing
coreBusWriteSimple snatT (Just addr) din = left <|> right where
    vin =  (unconcatI $ takei $ bv2v din) :: Vec a2 (Vec 32 Bit)
    left = coreBusWriteVec addr $ unpack . v2bv <$> vin 
    rD = v2bv $ drop (mulSNat (t42 snatT) d32) $ bv2v din :: BitVector a3
    right = coreBusWriteSingle (addr == (fromInteger $ snatToInteger $ lengthS vin)) rD
    takei  = takeI ::  (Vec a1 Bit) -> Vec (a2 * 32) Bit


-- write wide and narrow values on bus
coreBusWriteGeneric :: forall a a1 a2 a3 a4 adr. 
                      ( BusBuildC a a1 a2 a3 a4 , KnownNat a4, Eq adr, Num adr)
                   => SNat4 a1 a2 a3 a4
                   -> Signal ( Maybe (adr,a) )
                   -> Signal ( Maybe FullDataOut )
coreBusWriteGeneric snatT sigIn = fmap ( >>= readBus ) sigIn where
      shrAdd (addr,d) = (aSh,pack d)  :: (adr,BitVector a1) where
        aSh = addr
      rV (addr,d) = 
        if not (snatToInteger (t42 snatT)  == 0 ) then 
         coreBusWriteSimple snatT (Just addr) d
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
           => ((SNat a1, SNat a2, SNat a3, SNat a4),
               (SNat b1, SNat b2, SNat b3, SNat b4)) 
           -> ( Signal a -> Signal  b ) 
           -> Signal BusIn
           -> Signal BusOut
busBuild types core busInput = sigOut
  where   
    (snatIn,snatOut) = types 
    busIn = fmap f <$> busInput where
       f (addr,dir,datum) = (addr `shiftR` 2 ,dir,datum)
    -- bus to core
    sigInWide = coreBusReadGeneric snatIn busIn  
 
    -- core output back to bus
    sigOutWide = core sigInWide :: Signal b
    sigReadData :: Signal (Maybe (FullAddress,b))
    sigReadData = f <$> busIn <*> sigOutWide where
      f Nothing _ = Nothing
      f (Just (_,Write,_)) _ = Nothing
      f (Just (fullAddr,Read,_)) d = Just (fullAddr,d)
    
    sigOutBus = coreBusWriteGeneric snatOut sigReadData :: Signal (Maybe FullDataOut)

    sigOut = (<|>) <$> sigOutBus <*> signal (Just 0) -- allways valid


busE :: forall a a1 b b1.(BuildThC a , BuildThC b )
       => (Signal a -> Signal b) 
       -> Exp
busE f =  TupE $ TupE . fmap snatT <$> [ra,rb] where
            snatT n = SigE (VarE 'snat ) 
                           (AppT (ConT ''SNat) 
                           (LitT (NumTyLit n)))
            fr a1 = r where
                (a2,a3) = quotRem a1 32
                (ra2,ra3) | a3 == 0 = (a2-1,32)
                          | otherwise = (a2,a3)
                r = [a1,ra2,ra3,32-ra3]
            sa :: SNat (BitSize a)
            sa = snat
            ra = fr (snatToInteger sa)
            sb :: SNat (BitSize b)
            sb = snat
            rb = fr $ snatToInteger sb
            

-- | Template that helps deducing first argument of function 'busBuild'
bTQ:: (BitPack b, BitPack a, KnownNat (BitSize a),
      KnownNat (BitSize b)) =>
     (Signal a -> Signal b) -> ExpQ
bTQ = return . busE
