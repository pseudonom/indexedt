{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

import Control.Concurrent (threadDelay, myThreadId)
import Control.Monad.Indexed.Syntax
import Prelude hiding ((>>), (>>=), (=<<))

import Control.Monad.Trans.Indexed.Log

data Unknown = Unknown
data Print = Print
data Wait = Wait

printI :: (Show a) => a -> Performs Print IO i ()
printI = logLift Print . print

putStrLnI :: String -> Performs Print IO i ()
putStrLnI = logLift Print . putStrLn

threadDelayI :: Int -> Performs Wait IO i ()
threadDelayI = logLift Wait . threadDelay

main :: IO ()
main = runIndexedLogT mainI

mainI :: IndexedLogT IO '[] '[Print, Wait, Unknown, Print] ()
mainI = do
  printI 'c'
  -- We end up with a linear log even if we nest
  _ <- do
    i <- logLift Unknown myThreadId
    threadDelayI 1000000
    printI i
  printI 'a'
