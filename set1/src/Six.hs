module Six where

    import qualified Data.ByteString as B
    import Data.Bits (xor, popCount)
    import Data.List (sortBy)
    import Two (pad)
    import Three (englishFrequency, singleCharacter, makeKey)

    hammingDistance :: B.ByteString -> B.ByteString -> Int
    hammingDistance a b = sum $ map popCount $ B.zipWith xor a b

    windowBytes :: B.ByteString -> Int -> [B.ByteString] -> [B.ByteString]
    windowBytes str l acc = case str == B.empty of
                               True -> reverse acc
			       False -> windowBytes (B.drop l str) l ((B.take l str) : acc)

    keysizeGuesser :: B.ByteString -> Int -> (Double, Int)
    keysizeGuesser str l = ((fromIntegral $ hammingDistance a b) / (fromIntegral l), l)
                       where [a, b] = take 2 $ windowBytes str l []

    keysizeGenerator :: B.ByteString -> [Int]
    keysizeGenerator str = take 4 $ map snd $ sortBy (\(a, _) (b, _) -> compare a b) $ map (keysizeGuesser str) [2..40]

    zippy = B.zipWith (\a b -> B.concat (map B.singleton [a, b]))

    transpose :: [B.ByteString] -> [B.ByteString]
    transpose (x:xs) = foldr zippy [x] xs
    
    keys l = map (flip makeKey $ l) singleCharacter

    forOneSize str l = let
                          kk = keys l
                          --blocks = transpose . windowBytes str l []
			  --padded = map ((map . map) pad blocks) kk
			  --sorted = map (snd $ head $ englishFrequency) padded
			in
			  kk

--    padding lista l = let
--                          kk = keys l
--			  padded = (map . map) pad lista
--			  forAllKeys = transpose $ concat $ map (padded k) keys
--			  sorted = snd $ head $ map englishFrequency forAllKeys
--		      in
--		          sorted
                          
