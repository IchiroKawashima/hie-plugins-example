module Main where

import           Data.Proxy

import           Lib

main :: IO ()
main = do
    print $ knownNatExample (Proxy @3)
