{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE FlexibleContexts      #-}

module MatrixMul3x3

where
import CLaSH.Prelude
import MatrixMultiply


-- | 3x3 type
type M3x3 = Matrix 3 3 (Signed 16)

-- | combinatorial circuit
matrixMul3x3 :: (M3x3,M3x3) -> M3x3
matrixMul3x3 (a,b) = matrixMultiply a b

-- | sequential circuit (same circuit as matrixMul3x3, different type signature)
matrixMul3x3sig :: Signal (M3x3,M3x3) -> Signal M3x3
matrixMul3x3sig = fmap matrixMul3x3 


