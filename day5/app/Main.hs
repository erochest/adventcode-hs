{-# LANGUAGE NamedFieldPuns #-}

module Main where

import           Advent
import           Control.Arrow
import qualified Data.ByteString.Builder    as B
import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.Char
import qualified Data.HashSet               as S
import           Data.Traversable

main :: IO ()
main = interactAll partTwo

-- Part One

partOne :: BS.ByteString -> B.Builder
partOne = B.intDec
        . length
        . takeStable
        . iterate react
        . readPolymer
        . BS.filter isAlpha

-- Part Two

partTwo :: BS.ByteString -> B.Builder
partTwo = B.intDec
        . minimum
        . map (length . reactAll . uncurry removeUnitType)
        . sequenceA
        . (id &&& S.toList . getUnitTypeSet)
        . readPolymer
        . BS.filter isAlpha

-- Shared

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
    prepend Nothing us  = us
    prepend (Just u) us = u:us

reactAll :: Polymer -> Polymer
reactAll = takeStable . iterate react

takeStable :: Eq a => [a] -> a
takeStable (a:b:cs)
  | a == b = a
  | otherwise = takeStable (b:cs)

getUnitTypeSet :: Polymer -> S.HashSet Char
getUnitTypeSet = S.fromList . map unitType

removeUnitType :: Polymer -> Char -> Polymer
removeUnitType polymer c = filter ((/= c) . unitType) polymer
