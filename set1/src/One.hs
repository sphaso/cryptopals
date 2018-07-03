module One where
    import Data.ByteString.Char8 (pack, unpack)
    import Data.ByteString.Base16 as H (decode)
    import Data.ByteString.Base64 as B (encode)

    hexToBase64 :: String -> String
    hexToBase64 = unpack . B.encode . fst . H.decode . pack
