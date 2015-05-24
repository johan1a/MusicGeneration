{-# LANGUAGE OverloadedStrings #-}


import Data.Attoparsec.ByteString.Char8
import Data.Word
import Control.Applicative
-- We import ByteString qualified because the function
-- 'Data.ByteString.readFile' would clash with
-- 'Prelude.readFile'.

--import Data.ByteString.Char8
import qualified Data.ByteString as B

import Debug.Trace

inputFile :: FilePath
inputFile = "chorales2.lisp"


type Choral = [Note]
data Note =
  Note { 
             st :: Integer,
             pitch :: Integer,
             dur :: Integer,
             keySig :: Integer,
             timeSig :: Integer,
             fermata :: Integer
             } deriving Show


main :: IO ()
--main = print $ parseOnly noteParser "((st 12) (pitch 71) (dur 4) (keysig 4) (timesig 16) (fermata 0))"
main = B.readFile inputFile >>= print . parseOnly choralParser


choralParser :: Parser Choral
choralParser = do
    lparen
    skipSpace 
    number <- decimal
    skipSpace  
    notes <- noteParser
    choral <- many $ noteParser <* endOfLine
    skipSpace
    rparen
    skipSpace
    return choral

noteParser :: Parser Note
noteParser = do
    lparen
    skipSpace 
    st <- stParser
    skipSpace 
    pitch <- pitchParser
    skipSpace 
    dur <- durationParser
    skipSpace 
    keySig <- keySigParser
    skipSpace 
    timeSig <- timeSigParser
    skipSpace 
    fermata <- fermataParser
    skipSpace 
    rparen
    return $ Note st pitch dur keySig timeSig fermata

noteFeatureParser :: B.ByteString -> Parser Integer
noteFeatureParser s = do
    lparen
    skipSpace 
    string $ s--pack s
    skipSpace 
    val <- decimal
    skipSpace 
    rparen
    return val

stParser :: Parser Integer
stParser = noteFeatureParser "st"

pitchParser :: Parser Integer
pitchParser = noteFeatureParser "pitch"

durationParser :: Parser Integer
durationParser = noteFeatureParser "dur"

keySigParser :: Parser Integer
keySigParser = noteFeatureParser "keysig"

timeSigParser :: Parser Integer
timeSigParser = noteFeatureParser "timesig"

fermataParser :: Parser Integer
fermataParser = noteFeatureParser "fermata"

lparen :: Parser Char
lparen = char '('

rparen :: Parser Char
rparen = char ')'


-- skipSpaceNoNewline = skipWhile (\x -> isSpace_w8 x && not (isEndOfLine x))