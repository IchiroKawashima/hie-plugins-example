module Main where

import           Data.Proxy

import           Lib

main :: IO ()
main = do
    print (knownNatExample @3)
    print (natNormalizeExample @5 @7)
