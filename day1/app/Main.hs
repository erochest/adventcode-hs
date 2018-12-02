module Main where

import Data.Maybe (mapMaybe)
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.ByteString.Lazy.Builder as B

main :: IO ()
main = BS.interact
     $ B.toLazyByteString
     . (<> B.charUtf8 '\n')
     . B.intDec
     . sum
     . map fst
     . mapMaybe BS.readInt
     . BS.lines
