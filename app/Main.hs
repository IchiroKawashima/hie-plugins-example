module Main where

import           Data.Proxy

import           Lib

main :: IO ()
main = print $ someFunc (Proxy @3)
