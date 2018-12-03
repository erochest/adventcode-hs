module Main where

import qualified Data.ByteString.Lazy.Builder as B
import qualified Data.ByteString.Lazy.Char8   as BS
import           Data.Foldable
import qualified Data.HashSet                 as S
import qualified Data.List                    as L
import           Data.Maybe
import           Data.Traversable

main :: IO ()
main = BS.interact
     $ B.toLazyByteString
     . (<> B.charUtf8 '\n')
     . B.intDec
     . fromMaybe 0
     . sumUntilDuplicate
     . L.cycle
     . map fst
     . mapMaybe BS.readInt
     . BS.lines

sumUntilDuplicate :: [Int] -> Maybe Int
sumUntilDuplicate =
    listToMaybe
  . catMaybes
  . map (\(_, _, x) -> x)
  . L.scanl addFrequency (S.singleton (0 :: Int), 0, Nothing)

addFrequency :: (S.HashSet Int, Int, Maybe Int) -> Int -> (S.HashSet Int, Int, Maybe Int)
addFrequency (seen, accum, _) x
  | accum' `S.member` seen = (seen, accum', Just accum')
  | otherwise = (S.insert accum' seen, accum', Nothing)
  where
    accum' = x + accum

