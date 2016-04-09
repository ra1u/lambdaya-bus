module MatrixMultiplyCore3x3

where

import MatrixMul3x3
import System.RedPitaya.Bus
import CLaSH.Prelude
 


-- | makes simple bus out of core. first argument 
-- is template haskell (TH) for type deduction
-- becaues of TH we can not define matrixMul3x3sig in this file
simpleBus :: Signal BusIn -> Signal BusOut
simpleBus = busBuild $(bTQ matrixMul3x3sig) matrixMul3x3sig


-- | generate core with apropriate names to be included in RedPitaya
{-# ANN topEntity defTopRedPitayaSimple #-}
topEntity = rpSimpleBind simpleBus 
