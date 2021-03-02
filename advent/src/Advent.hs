module Advent
    ( interact
    , interactAll
    , parse
    )
where

import qualified Data.ByteString.Builder       as B
import qualified Data.ByteString.Lazy.Char8    as BS
import           Prelude                 hiding ( interact )
import           Data.Attoparsec.ByteString.Char8
                                         hiding ( parse )

interact :: ([BS.ByteString] -> B.Builder) -> IO ()
interact f =
    BS.interact
        $ B.toLazyByteString
        . (<> B.charUtf8 '\n')
        . f
        . filter (not . BS.null)
        . BS.lines

interactAll :: (BS.ByteString -> B.Builder) -> IO ()
interactAll f = BS.interact $ B.toLazyByteString . (<> B.charUtf8 '\n') . f

parse :: Parser a -> BS.ByteString -> Either String a
parse p = parseOnly p . BS.toStrict
