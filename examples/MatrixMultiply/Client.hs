{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE TemplateHaskell       #-}

module Main where

import qualified Prelude as P
import MatrixMul3x3
import System.RedPitaya.Bus
import CLaSH.Prelude
import System.RedPitaya.Fpga
import System.RedPitaya.Tcp
import Control.Monad.IO.Class
import System.Environment



matA = (1 :> 2 :> 3 :> Nil) :> 
       (4 :> 5 :> 6 :> Nil) :> 
       (7 :> 8 :> 9 :> Nil) :> Nil :: M3x3

matB = (10 :> 11 :> 12 :> Nil) :> 
       (13 :> 14 :> 15 :> Nil) :> 
       (16 :> 17 :> 18 :> Nil) :> Nil :: M3x3


multiply :: (M3x3,M3x3) -> NetworkFpgaSetGet M3x3
multiply = callCore $(bTQ matrixMul3x3sig) 5 0 5 0

rpPort = 4242
rpIp = "10.42.0.11"

main = runRemoteRp rpIp 4242 $ do
    m <- multiply (matA,matB)
    let p = liftIO . P.putStrLn . P.show
    p matA
    p matB
    p m -- matA * matB
