module Set1 where

import Test.Hspec
import Test.QuickCheck
import qualified Data.ByteString as B
import Data.ByteString.Char8 (pack, unpack)
import Support (fourthInput)
import One (hexToBase64)
import Two (fromHex, toHex, pad)
import Three (singleCharacter, makeKey, englishFrequency)
import Four (runThrough)
import Five (run)
import Six (hammingDistance)

firstExercise :: Spec
firstExercise = do
    describe "first exercise" $ do
       it "from hex to base64" $ do
           hexToBase64 "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d" `shouldBe` "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t"


    describe "second exercise" $ do
         it "padding" $ do
                let
                    a = fromHex "1c0111001f010100061a024b53535009181c"
                    b = fromHex "686974207468652062756c6c277320657965"
                    padded = pad a b
                 in
                    toHex padded `shouldBe` "746865206b696420646f6e277420706c6179"


    describe "third exercise" $ do
        it "single character key" $ do
            (unpack $ makeKey (pack "a") 3) `shouldBe` "aaa"

        it "multiple characters key" $ do
            (unpack $ makeKey (pack "ICE") 3) `shouldBe` "ICEICEICE"

        it "decrypt" $ do
               let
                    a = fromHex "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"
                    keys = map (flip makeKey $ B.length a) singleCharacter
                    padded = map (pad a) keys
                    sorted = snd $ head $ englishFrequency padded
                in
                    unpack sorted `shouldBe` "Cooking MC's like a pound of bacon"

    describe "fourth exercise" $ do
        it "decrypt" $ do
               let
                   a = runThrough fourthInput
                in
                   unpack a `shouldBe` "Now that the party is jumping\n"

    describe "fifth exercise" $ do
          it "decrypt" $ do
            let
                f = run "Burning 'em, if you ain't quick and nimble\nI go crazy when I hear a cymbal" "ICE"
              in
                toHex f `shouldBe`       "0b3637272a2b2e63622c2e69692a23693a2a3c6324202d623d63343c2a26226324272765272a282b2f20430a652e2c652a3124333a653e2b2027630c692b20283165286326302e27282f"


    describe "sixth exercise" $ do
        it "hamming distance" $ do
            hammingDistance (pack "this is a test") (pack "wokka wokka!!!") `shouldBe` 37

