module Main where

import qualified Data.ByteString.Lazy.Builder as B
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.HashMap.Strict as M
import Data.Monoid
import Data.Foldable
import Data.Hashable
import Control.Arrow

main :: IO ()
main = BS.interact
     $ B.toLazyByteString
     . (<> B.charUtf8 '\n')
     . B.intDec
     . uncurry (*)
     . foldl' addIfPair (0, 0)
     . fmap twosAndThrees
     . fmap (BS.foldl' insert mempty)
     . BS.lines

newtype Counts a = Counts { getCounts :: M.HashMap a (Sum Int) }

instance (Eq a, Hashable a) => Semigroup (Counts a) where
  (Counts a) <> (Counts b) = Counts $ M.unionWith (<>) a b

instance (Eq a, Hashable a) => Monoid (Counts a) where
  mempty = Counts M.empty

insert :: (Eq a, Hashable a) => Counts a -> a -> Counts a
insert (Counts c) x = Counts $ M.insertWith mappend x (Sum 1) c

twosAndThrees :: Counts a -> (Bool, Bool)
twosAndThrees (Counts a) = (2 `elem` values, 3 `elem` values)
  where
    values = M.elems a

addIf :: Int -> Bool -> Int
addIf x True = x + 1
addIf x False = x

addIfPair :: (Int, Int) -> (Bool, Bool) -> (Int, Int)
addIfPair (x, y) (a, b) = (addIf x a, addIf y b)
