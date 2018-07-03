module Two where
    import qualified Data.ByteString as B
    import Data.ByteString.Char8
    import Data.ByteString.Base16 as H (encode, decode)
    import Data.Bits (xor)

    fromHex :: String -> ByteString
    fromHex = fst . H.decode . pack

    toHex :: ByteString -> String
    toHex = unpack . H.encode

    pad :: ByteString -> ByteString -> ByteString
    pad a b = B.pack $ B.zipWith xor a b
