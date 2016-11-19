
module MatrixMul 
where

import CLaSH.Prelude


-- | Type representing matrix of size m*n with element type t
type Matrix n m t = Vec n (Vec m t)
-- type MatrixCore a b c t = Matrix a b t -> Matrix b c t -> Matrix a c t
type MatrixCore a b c t = Signal (Matrix a b t , Matrix b c t) -> Signal (Matrix a c t)

-- | Multiply two matrices using combinatorial 
-- | circuit using a*(b+1)*c multipliers
matrixMultiply :: (KnownNat a, KnownNat (b+1), KnownNat c,Num t) => 
       Matrix a (b+1) t -> 
       Matrix (b+1) c t ->  
       Matrix a c t
matrixMultiply a b = fmap ( mulLine (traverse id b)) a
    where 
        mulLine a b = fmap (fold (+) . zipWith (*) b) a



