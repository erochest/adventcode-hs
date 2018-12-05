{-# LANGUAGE NamedFieldPuns #-}

-- 10586
-- 10585

module Main where

import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.ByteString.Builder as B
import Advent
import Data.Char

main :: IO ()
main = interactAll
     $ B.intDec
     . length
     . takeStable
     . iterate react
     . readPolymer
     . BS.filter isAlpha

data Unit = Unit { unitType:: !Char, unitPositive:: !Bool }
  deriving (Show, Eq, Ord)

type Polymer = [Unit]

doReact :: Unit -> Unit -> Bool
doReact a b =  unitType a == unitType b
            && unitPositive a /= unitPositive b

readPolymer :: BS.ByteString -> Polymer
readPolymer = map readUnit . BS.unpack

readUnit :: Char -> Unit
readUnit c = Unit (toLower c) (isUpper c)

showPolymer :: Polymer -> String
showPolymer = map showUnit

showUnit :: Unit -> Char
showUnit Unit{unitType, unitPositive}
  | unitPositive = toUpper unitType
  | otherwise = unitType

react :: Polymer -> Polymer
react = uncurry prepend . foldr step (Nothing, [])
  where
    step :: Unit -> (Maybe Unit, [Unit]) -> (Maybe Unit, [Unit])
    step current (Nothing, us) = (Just current, us)
    step current (Just prev, us)
      | doReact current prev = (Nothing, us)
      | otherwise = (Just current, prev:us)

    prepend :: Maybe Unit -> [Unit] -> [Unit]
    prepend Nothing us = us
    prepend (Just u) us = u:us

takeStable :: Eq a => [a] -> a
takeStable (a:b:cs)
  | a == b = a
  | otherwise = takeStable (b:cs)
