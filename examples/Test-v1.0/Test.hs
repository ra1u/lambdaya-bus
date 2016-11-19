{-# LANGUAGE TemplateHaskell      #-}

import CLaSH.Prelude hiding ((++),undefined,take)
import Prelude
import MatrixMul
import System.RedPitaya.Bus.Tools
import System.RedPitaya.Bus

type MM = Matrix 3 3 (Signed 8)

mm = fmap $ Just . uncurry matrixMultiply  :: Signal (MM,MM) -> Signal (Maybe MM)
core inp = busBind n mm (addrLow inp) where
   n = $(bTQ (undefined :: MatrixCore 3 3 3 (Signed 8))) -- type deduce template

topEntity = rpSimpleBind core

i2i = fromInteger . toInteger
-- test
wr add val =  Just (i2i add,Write,i2i val) :: BusIn
wr5 = let f x = wr x  (x * 0x01010101 + 0x10203) in f <$> [0 .. 5]
rd = let f x = Just (x,Read,0) in fmap f [0,1 .. 2]
testHelper = take 9 $ simulate core (wr5 ++ rd)
