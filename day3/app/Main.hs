{-# LANGUAGE RecordWildCards #-}

module Main where

import           Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Builder          as B
import qualified Data.ByteString.Lazy.Char8       as BS
import qualified Data.HashSet                     as S
import           Prelude                          hiding (interact)

main :: IO ()
main = interact
     $ B.intDec
     . S.size
     . S.fromList
     . getAllOverlaps
     . either error id
     . mapM parseClaim
     . filter ((> 0) . BS.length)

interact :: ([BS.ByteString] -> B.Builder) -> IO ()
interact f = BS.interact $ B.toLazyByteString . (<> B.charUtf8 '\n') . f . BS.lines

type Coord = (Int, Int)
data Claim
  = Claim
    { claimId         :: !Int
    , claimCoordinate :: !Coord
    , claimSize       :: !(Int, Int)
    } deriving (Show, Eq, Ord)

parseClaim :: BS.ByteString -> Either String Claim
parseClaim = parseOnly (Claim <$> id <*> coordinate <*> size) . BS.toStrict
  where
    id = char '#' *> decimal <* skipSpace
    coordinate = char '@' *> skipSpace *> pair ','
    size = char ':' *> skipSpace *> pair 'x'
    pair c = (,) <$> decimal <*> (char c *> decimal)

getOverlaps :: Claim -> Claim -> [Coord]
getOverlaps a@Claim{claimId=idA} b@Claim{claimId=idB}
  | idA == idB = []
  | otherwise = filter (b `contains`) $ getSquares a

getSquares :: Claim -> [Coord]
getSquares Claim{claimCoordinate=(x, y), claimSize=(width, height)} = do
  dy <- [0..height-1]
  dx <- [0..width-1]
  return (x+dx, y+dy)

contains :: Claim -> Coord -> Bool
contains Claim{claimCoordinate=(left, top), claimSize=(width, height)} (x, y) =
  x >= left && x < (left + width) && y >= top && y < (top + height)

getAllOverlaps :: [Claim] -> [Coord]
getAllOverlaps claims = concatMap (getAllOverlaps' claims) claims
  where
    getAllOverlaps' :: [Claim] -> Claim -> [Coord]
    getAllOverlaps' claims claim = concatMap (getOverlaps claim) claims
