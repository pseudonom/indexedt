{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RebindableSyntax #-}

import Control.Concurrent (threadDelay)
import Control.Monad.Trans.Indexed.Log
import Prelude hiding ((>>), (>>=), (=<<))

data Print = Print
data ReadIn = ReadIn

printI :: (Show a) => a -> Performs Print i IO ()
printI = logLift Print . print

putStrLnI :: String -> Performs Print i IO ()
putStrLnI = logLift Print . putStrLn

getLineI :: Performs ReadIn i IO String
getLineI = logLift ReadIn getLine

main :: IO ()
main = runIndexedLogT mainI

mainI :: IndexedLogT '[] '[Print, Unknown, ReadIn, Print] IO ()
mainI = do
  printI 'c'
  x <- getLineI
  logLift Unknown . threadDelay $ round (1e6 :: Double)
  putStrLnI x
  printI 'a'
