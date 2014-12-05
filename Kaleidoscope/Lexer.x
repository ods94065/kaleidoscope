{
module Kaleidoscope.Lexer (
  scan, scanFile, scanInput,
  Token(..), Tok(..), Posn(..), numLitValue, idValue
  ) where

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy.Char8 as LC
import Control.Applicative ((<$>))
}

%wrapper "posn-bytestring"

$digit = 0-9
$alpha = [a-zA-Z]

tokens :-
  $white+ ;
  \# .* ;
  "def" { tok Def }
  "extern" { tok Extern }
  \+ { tok Plus }
  \- { tok Minus }
  \* { tok Star }
  \/ { tok Slash }
  \; { tok Semicolon }
  \, { tok Comma }
  \( { tok LParen }
  \) { tok RParen }
  \< { tok LAngle }
  ($digit+ (\. $digit*)?) | (\. $digit+) { mkNumLit }
  $alpha [$alpha $digit \_]* { mkId }

{
data Posn = Posn !Int !Int deriving (Eq, Show)
data Token = Token { tTok :: !Tok, tPos :: !Posn } deriving (Eq, Show)
data Tok = 
  Def | Extern | Plus | Minus | Star | Slash | Semicolon | Comma
  | LParen | RParen | LAngle | NumLit Double | Id S.ByteString
  deriving (Eq, Show)

numLitValue :: Tok -> Double
numLitValue (NumLit d) = d
numLitValue _ = error "numLitValue expected NumLit"

idValue :: Tok -> S.ByteString
idValue (Id s) = s
idValue _ = error "idValue expected Id"

scan :: L.ByteString -> [Token]
scan = alexScanTokens

scanFile :: FilePath -> IO [Token]
scanFile p = scan <$> L.readFile p

scanInput :: IO [Token]
scanInput = scan <$> L.getContents

toPosn :: AlexPosn -> Posn
toPosn (AlexPn off lno cno) = Posn lno cno

tok :: Tok -> AlexPosn -> L.ByteString -> Token
tok t p _ = Token t (toPosn p)

mkNumLit :: AlexPosn -> L.ByteString -> Token
mkNumLit p s = Token (strToNumLit s) (toPosn p)
  where
    strToNumLit = NumLit . read . addInitialZeroIfNecessary . LC.unpack
    -- this is necessary because Haskell requires digits
    -- before the decimal point
    addInitialZeroIfNecessary s@('.' : cs) = '0' : s
    addInitialZeroIfNecessary cs = cs

mkId :: AlexPosn -> L.ByteString -> Token
mkId p s = Token (Id (L.toStrict s)) (toPosn p)
}
