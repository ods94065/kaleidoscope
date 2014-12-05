module Main where

import Kaleidoscope.Lexer
import Data.Functor ((<$>))
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as S
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)

getLineLazy :: IO L.ByteString
getLineLazy = strictToLazy <$> S.getLine
  where
    strictToLazy s = L.fromChunks [s]

main :: IO ()
main = do
  s <- getLineLazy
  let toks = scan s
  putStrLn $ show toks
  main
