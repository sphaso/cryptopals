module Three where
    import Two (fromHex, pad)
    import Data.ByteString.Char8 as C (pack, filter, head)
    import qualified Data.ByteString as B
    import Data.Char (toUpper)
    import Data.List (sortBy)

    letters :: String
    letters = "abcdefghijklmnopqrstuvwxyz"

    numbers :: String
    numbers = "1234567890"

    singleCharacter :: [B.ByteString]
    singleCharacter = map pack $ map (:[]) (letters ++ (map toUpper letters) ++ numbers)

    makeKey :: B.ByteString -> Int -> B.ByteString
    makeKey c l = B.concat $ take l $ repeat c

    singleCharPad :: B.ByteString -> [B.ByteString] -> [B.ByteString]
    singleCharPad s chars =
                             let
                               ll = B.length s
                               keys = map (flip makeKey $ ll) chars
                             in
                               map (pad s) keys

    
    sortedByFrequency :: [(Double, B.ByteString)] -> Char -> [(Double, B.ByteString)]
    sortedByFrequency ll c = let
                               f = map (flip frequency $ c) ll
                             in
                               sortBy (\(a, _) (b, _) -> compare b a) f

    frequency :: (Double, B.ByteString) -> Char -> (Double, B.ByteString)
    frequency (a, s) c = 
                    let
                       ll = fromIntegral $ B.length s
                       chra = fromIntegral $ B.length $ C.filter ((==c) . toUpper) s
                    in
                       (a + chra / ll, s)

    englishFrequency :: [B.ByteString] -> [(Double, B.ByteString)]
    englishFrequency pads =
                            let
                                lets = "ETAOIN SHRDLU"
                                reduced = foldr (\c acc -> sortedByFrequency acc c) (map (\a -> (0, a))pads) lets
                            in
                                reduced
