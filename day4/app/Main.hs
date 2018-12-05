{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Advent
import           Attoparsec.Time.ByteString
import           Control.Arrow
import           Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Builder          as B
import qualified Data.ByteString.Lazy.Char8       as BS
import           Data.Foldable
import           Data.Hashable
import qualified Data.HashMap.Strict              as M
import qualified Data.HashSet                     as S
import qualified Data.List                        as L
import           Data.Monoid
import           Data.Ord
import           Data.Time


main :: IO ()
main = Advent.interact partOne

-- partOne

partOne :: [BS.ByteString] -> B.Builder
partOne = B.intDec
        . uncurry (*)
        . second getMinuteAsleepMost
        . maximumBy (comparing $ timeAsleep . snd)
        . M.toList
        . compileTable
        . L.sortBy (comparing obsDateTime)
        . either error id
        . mapM parseLog
        . filter (not . BS.null)

data Log
  = Log
    { obsDateTime :: !UTCTime
    , obsState    :: !LogState
    } deriving (Show, Eq)

data LogState
  = Guard !GuardID
  | FallsAsleep
  | WakesUp
  deriving (Show, Eq, Ord)

type GuardID = Int
type GuardTable = M.HashMap GuardID DateTable
type MonthDay = (Int, Int)
type DateTable = M.HashMap MonthDay SleepTable
type Minute = Int
type SleepTable = S.HashSet Minute
type SleepCounts = CountMap Minute

newtype CountMap a = CountMap { getMap :: M.HashMap a (Sum Int) }

instance (Eq a, Hashable a) => Semigroup (CountMap a) where
  (CountMap a) <> (CountMap b) = CountMap $ M.unionWith (<>) a b

instance (Eq a, Hashable a) => Monoid (CountMap a) where
  mempty = CountMap M.empty

incr :: (Eq a, Hashable a) => a -> CountMap a -> CountMap a
incr k (CountMap c) = CountMap $ M.insertWith (<>) k (Sum 1) c

insertRangeAsleep :: GuardID -> UTCTime -> UTCTime -> GuardTable -> GuardTable
insertRangeAsleep gid asleep awake = M.alter insertDateTable gid
  where
    insertDateTable :: Maybe DateTable -> Maybe DateTable
    insertDateTable =
      Just . maybe (M.singleton dateKey sleepTable)
                   (M.insertWith S.union dateKey sleepTable)

    (_, month, day) = toGregorian $ utctDay asleep

    dateKey :: MonthDay
    dateKey = (month, day)

    getMinutes = todMin . timeToTimeOfDay . utctDayTime

    sleepTable :: SleepTable
    sleepTable = S.fromList [getMinutes asleep .. (getMinutes awake) - 1]

parseLog :: BS.ByteString -> Either String Log
parseLog input = left (++ ": " ++ BS.unpack input)
               $ parseOnly line
               $ BS.toStrict input
  where
    line = Log <$> dateTime <*> (skipSpace *> state)
    dateTime = char '[' *> utcTime <* char ']'
    utcTime =   UTCTime <$> (dayInISO8601 <* skipSpace) <*> time
    time =   fmap secondsToDiffTime
         (   (+)
         <$> (fmap (* 3600) decimal)
         <*> (char ':' *> (fmap (* 60) decimal)))
    state = choice
          [ string "Guard #" *> (Guard <$> decimal) <* string " begins shift"
          , string "falls asleep" *> pure FallsAsleep
          , string "wakes up" *> pure WakesUp
          ]

compileTable :: [Log] -> GuardTable
compileTable = thd . foldl' step (Nothing, Nothing, M.empty)
  where
    thd (_, _, x) = x

    step :: (Maybe GuardID, Maybe UTCTime, GuardTable)
         -> Log
         -> (Maybe GuardID, Maybe UTCTime, GuardTable)
    step (_, _, guardTable) Log{obsState=Guard gid} =
      (Just gid, Nothing, guardTable)
    step (jGid@(Just _), _, guardTable)
         Log{obsDateTime=dateTime, obsState=FallsAsleep} =
      (jGid, Just dateTime, guardTable)
    step (jGid@(Just gid), Just asleep, guardTable)
         Log{obsDateTime=wakesUp, obsState=WakesUp} =
      (jGid, Nothing, insertRangeAsleep gid asleep wakesUp guardTable)

timeAsleep :: DateTable -> Int
timeAsleep = sum . concatMap S.toList . M.elems

minutesAsleep :: [SleepTable] -> SleepCounts
minutesAsleep = foldl' step mempty
  where
    step :: SleepCounts -> SleepTable -> SleepCounts
    step counts = foldl' (flip incr) counts . S.toList

getMinuteAsleepMost :: DateTable -> Minute
getMinuteAsleepMost =
  fst . maximumBy (comparing snd) . M.toList . getMap . minutesAsleep . M.elems
