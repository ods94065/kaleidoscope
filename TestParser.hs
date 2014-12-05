module Main where

import Kaleidoscope.Parser

main :: IO ()
main = do
  m <- parseInput
  putStrLn $ show m
