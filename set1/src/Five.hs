module Five where

import qualified Data.ByteString as B
import Data.ByteString.Char8 (pack)
import Two (fromHex, toHex, pad)
import Three (makeKey)

run s k =
          let
	     bs = pack s
	     bk = pack k
             key = makeKey bk (B.length bs)
          in
	     pad bs key
