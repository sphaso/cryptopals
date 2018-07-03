module Four where

import qualified Data.ByteString as B
import Two (fromHex, toHex, pad)
import Three (singleCharacter, makeKey, englishFrequency)

singleLine l = 
               let
                   a = fromHex l 
                   keys = map (flip makeKey $ B.length a) singleCharacter
                   padded = map (pad a) keys
	       in
                   padded

runThrough ml =
                let
                    ll = concat $ map singleLine (lines ml)
		in
		    snd $ head $ englishFrequency ll
