{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RebindableSyntax #-}

import Control.Concurrent (threadDelay)
import Control.Monad.Indexed.Syntax
import Prelude hiding ((>>), (>>=), (=<<))

import Control.Monad.Trans.Indexed.Log

data Unknown = Unknown
data Print = Print
data ReadIn = ReadIn

printI :: (Show a) => a -> Performs Print IO i ()
printI = logLift Print . print

putStrLnI :: String -> Performs Print IO i ()
putStrLnI = logLift Print . putStrLn

getLineI :: Performs ReadIn IO i String
getLineI = logLift ReadIn getLine

main :: IO ()
main = runIndexedLogT mainI

mainI :: IndexedLogT IO '[] '[Print, Unknown, ReadIn, Print] ()
mainI = do
  printI 'c'
  x <- getLineI
  logLift Unknown . threadDelay $ round (1e6 :: Double)
  putStrLnI x
  printI 'a'
