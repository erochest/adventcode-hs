{-# LANGUAGE TupleSections #-}

module Main where

import           Control.Arrow
import qualified Data.ByteString.Lazy.Builder as B
import qualified Data.ByteString.Lazy.Char8   as BS
import           Data.Foldable
import           Data.Hashable
import qualified Data.HashMap.Strict          as M
import           Data.Maybe
import           Data.Monoid

main :: IO ()
main = BS.interact
     $ B.toLazyByteString
     . (<> B.charUtf8 '\n')
     . B.lazyByteString
     . uncurry onlySame
     . fromJust         -- This line will throw an error if no matches are found.
     . findMatches
     . BS.lines

findMatches :: [BS.ByteString] -> Maybe (BS.ByteString, BS.ByteString)
findMatches xs = listToMaybe $ concatMap (findMatches' xs) xs
  where
    findMatches' :: [BS.ByteString] -> BS.ByteString -> [(BS.ByteString, BS.ByteString)]
    findMatches' xs x = filter (uncurry matches) $ map (x,) xs

    matches :: BS.ByteString -> BS.ByteString -> Bool
    matches a b = (== 1) $ length $ filter (uncurry (/=)) $ BS.zip a b

onlySame :: BS.ByteString -> BS.ByteString -> BS.ByteString
onlySame a b = BS.pack $ map fst $ filter (uncurry (==)) $ BS.zip a b
