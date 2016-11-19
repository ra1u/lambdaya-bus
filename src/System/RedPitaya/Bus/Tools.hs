{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE MagicHash            #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TemplateHaskell      #-}

module System.RedPitaya.Bus.Tools
where

import CLaSH.Prelude as CP hiding (snat,v)
import qualified Prelude as P
import System.RedPitaya.Fpga (Registry)
import Language.Haskell.TH

-- Like replace, but if value is Nothing, nothing replaced
{-# INLINE replaceM #-}
replaceM :: (Enum a1,KnownNat n) 
         => Maybe a1 -> d -> Vec n d -> Vec n d
replaceM a d v = zipWith iff sv v where
    iff b r = if b then d
                   else r
    -- demux that has True on appropriate bit
    sv = replace addr (isJust a) falsev -- Vec n Bool
    addr = maybe (toEnum 0) id a 
    falsev = repeat False
    


{-# INLINE isJust #-}
isJust :: Maybe a -> Bool
isJust Nothing = False
isJust (Just _) = True

{-# INLINE fromMaybe #-}
fromMaybe :: a -> Maybe a -> a
fromMaybe _ (Just a) = a
fromMaybe a Nothing = a

-- Vector splitters
{-# INLINE u2reg #-}
u2reg :: Unsigned 32 -> Registry
u2reg = fromInteger . toInteger 

{-# INLINE reg2u #-}
reg2u :: Registry -> Unsigned 32
reg2u = fromInteger . toInteger 

{-# INLINE vec2split #-}
vec2split :: forall n . ( KnownNat n) => SNat n -> Vec (n*32) Bit -> [Unsigned 32]
vec2split _  = toList . map bitCoerce . unconcatI

{-# INLINE vec2splitI #-}
vec2splitI :: forall n . ( KnownNat n) => Vec (n*32) Bit -> [Unsigned 32]
vec2splitI = withSNat vec2split

-- 
reduceSize :: (KnownNat m, (n*32) ~ (m + k)) 
           => SNat n -> Vec (n*32) a -> Vec m a
reduceSize _ = takeI

--
extendSize :: (KnownNat k, Default a, (n*32) ~ (m + k)) 
           => SNat n -> Vec m a -> Vec (n*32) a 
extendSize _ = (++ repeat def)

-- tools for fromListI
fromListUnsafeU :: UNat n -> [a] -> Vec n a
fromListUnsafeU UZero  _  = Nil
fromListUnsafeU (USucc s) (x:xs) = x CP.:> fromListUnsafeU s xs
fromListUnsafeU (USucc _) [] = undefined 

fromListUnsafe :: SNat n -> [a] -> Vec n a
fromListUnsafe n = fromListUnsafeU (toUNat n)

fromListS :: (Default a ) =>  SNat n -> [a] -> Vec n a
fromListS n xs = fromListUnsafe n $ xs P.++  P.repeat def

fromListI :: (Default a ,KnownNat n) =>  [a] -> Vec n a
fromListI = withSNat fromListS



-- | Template haskell function
-- that generates Exp from function 
-- This returns singletons (type level naturals)
-- that is used to deduce appropriate width and how to split bus
busE :: forall a b .
      (KnownNat (BitSize a),KnownNat (BitSize b))
       => (Signal a -> Signal b) 
       -> Exp
busE _ =  TupE ( fmap snatT [an,bn] ) where
            snatT n = SigE (AppE (VarE 'withSNat) (VarE 'id)) 
                      (AppT (ConT ''SNat) (LitT (NumTyLit n)))
            f a | r == 0 = q
                | otherwise = q+1
                     where
                      (q,r) = quotRem a 32
            an = f $ snatToInteger (snat' :: SNat (BitSize a))
            bn = f $ snatToInteger (snat' :: SNat (BitSize b))


-- | Template that helps deducing first argument of function 'busBuild'
bTQ:: (KnownNat (BitSize a),
      KnownNat (BitSize b)) =>
     (Signal a -> Signal b) -> ExpQ
bTQ = return . busE

-- | same as CLaSH.Promoted.Nat.snat
-- except it does not generate deprecated warrning
{-# INLINE snat' #-}
snat' :: KnownNat n => SNat n
snat' = withSNat id
