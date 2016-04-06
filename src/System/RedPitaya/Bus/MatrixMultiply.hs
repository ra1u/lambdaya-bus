{-|
Copyright  :  Luka Rahne
License    :  LGPL-3 (see the file LICENSE)
Maintainer :  Luka Rahne <luka.rahne@gmail.com>
-}

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE FlexibleContexts      #-}

module System.RedPitaya.Bus.MatrixMultiply 
(
  Matrix(..),
  matrixMultiply
)
where
import CLaSH.Prelude as CP
import qualified Prelude

-- | Type representing matrix of size m*n with element type t
type Matrix n m t = Vec n (Vec m t)

-- | Multiply two matrices using combinatorial 
-- | circuit using a*(b+1)*c multipliers
matrixMultiply :: (KnownNat a, KnownNat (b+1), KnownNat c,Num t) => 
       Matrix a (b+1) t -> 
       Matrix (b+1) c t ->  
       Matrix a c t
matrixMultiply a b = fmap ( mulLine (traverse id b)) a
    where 
        mulLine a b = fmap (fold (+) . zipWith (*) b) a
