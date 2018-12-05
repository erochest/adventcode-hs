module Advent
    ( interact
    , interactAll
    ) where

import qualified Data.ByteString.Builder    as B
import qualified Data.ByteString.Lazy.Char8 as BS
import           Prelude                    hiding (interact)

interact :: ([BS.ByteString] -> B.Builder) -> IO ()
interact f = BS.interact $ B.toLazyByteString . (<> B.charUtf8 '\n') . f . BS.lines

interactAll :: (BS.ByteString -> B.Builder) -> IO ()
interactAll f = BS.interact $ B.toLazyByteString . (<> B.charUtf8 '\n') . f
