--TODO Change from String to Text
--TODO Treat errors appropriately

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE RankNTypes #-}

module RS256 (verifyGoogleJWT) where

import qualified Data.Text as T
import Data.String (fromString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Base64.URL as BS64
import qualified Data.ByteString.Char8 as C8

import Data.X509 (SignedCertificate, PubKey(PubKeyRSA), certPubKey, getCertificate)
import Data.X509.Memory (readSignedObjectFromMemory)

import Codec.Crypto.RSA (verify)
import qualified Crypto.Types.PubKey.RSA as RSA (PublicKey(PublicKey))
import Crypto.PubKey.RSA.Types (public_size, public_n, public_e)

import qualified Network.Wreq as WREQ
import Control.Lens
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson.Lens as DAL (_String, key)
import Data.Map as Map
import Data.Aeson ((.:?), decode, withObject, parseJSON, FromJSON, Value)
import Data.List.Split

import qualified Web.JWT as JWT

readSignedCertificateFromMemory :: B.ByteString -> [SignedCertificate]
readSignedCertificateFromMemory = readSignedObjectFromMemory

extractRSA (PubKeyRSA rsaPubKey) = RSA.PublicKey (public_size rsaPubKey) (public_n rsaPubKey) (public_e rsaPubKey)
extractRSA _ = error "Error!"

extractPubKey (x:[]) = extractRSA $ certPubKey $  getCertificate x
extractPubKey _ = error "Error!"

addPadding :: String -> String
addPadding bs64string = bs64string ++ (Prelude.concat $ Prelude.replicate (4 - ((length bs64string) `mod` 4)) "=")

getHeader :: [[Char]] -> [Char]
getHeader (x:_) = x
getHeader _ = error "Invalid JWT!"

getHeaderAndPayload :: [[Char]] -> [Char]
getHeaderAndPayload (x:y:_) = x ++ "." ++ y
getHeaderAndPayload _ = error "Invalid JWT!"

getSignature :: [[Char]] -> [Char]
getSignature (_:_:s:_) = addPadding s
getSignature _ = error "Invalid JWT!"

getHeaderWithPadding :: String -> String
getHeaderWithPadding jwt = if (length jwt) == 0 then error "Invalid JWT!" else addPadding $ getHeader $ splitOn "." jwt

data JWTHeader = JWTHeader {
    kid :: Maybe String
  , alg :: Maybe String
} deriving (Eq, Show)

instance FromJSON JWTHeader where
    parseJSON = withObject "JWTHeader"
                    (\o -> JWTHeader
                    <$> o .:? "kid"
                    <*> o .:? "alg")

getKidFromHeader :: Maybe JWTHeader -> String
getKidFromHeader h = do
    case h of
        Just headerValue -> do
            let headerKid = kid headerValue
            case headerKid of
                Just k -> k
                _ -> error "Invalid JWT header!"
        _ -> error "Invalid JWT header!"

data JWTPayloadSig = JWTPayloadSig {
    payload :: String
  , signature :: String
} deriving (Eq, Show)

extractPayloadSig :: String -> JWTPayloadSig
extractPayloadSig jwt = do
    let splittedJWT = splitOn "." jwt
    let p = getHeaderAndPayload splittedJWT
    let s = getSignature splittedJWT
    JWTPayloadSig p s

verifyGoogleJWT :: String -> IO (Bool)
verifyGoogleJWT jwt = do
    -- let jwt = "eyJhbGciOiJSUzI1NiIsImtpZCI6ImM2ZjBlZTE2YmU3MGM0ODhkZDM5ZGI3MGY2ZjRkMTM3YTA0ODkxZTMifQ.eyJhenAiOiI1ODI0MzczNzE4NDctdHJza2hubWdzaXFtcmZyZGJ1cmxlZG44anVvMWVzc3QuYXBwcy5nb29nbGV1c2VyY29udGVudC5jb20iLCJhdWQiOiI1ODI0MzczNzE4NDctdHJza2hubWdzaXFtcmZyZGJ1cmxlZG44anVvMWVzc3QuYXBwcy5nb29nbGV1c2VyY29udGVudC5jb20iLCJzdWIiOiIxMDYzMjAxODYzMDAzNDQ1ODcyNzEiLCJoZCI6ImNjYy51ZmNnLmVkdS5iciIsImVtYWlsIjoibGVvLnZpdGFsQGNjYy51ZmNnLmVkdS5iciIsImVtYWlsX3ZlcmlmaWVkIjp0cnVlLCJhdF9oYXNoIjoibGxKRGFlcHVVYlFteVBwWS1lbnZQUSIsImV4cCI6MTUyMTYwMDQ2MywiaXNzIjoiYWNjb3VudHMuZ29vZ2xlLmNvbSIsImp0aSI6IjhmMTFkNDU5MTgwZmI3NTExY2UwNDc2Zjc4NmFlNDU1YzJjMTVjOTAiLCJpYXQiOjE1MjE1OTY4NjMsIm5hbWUiOiJMZW8gVml0YWwiLCJwaWN0dXJlIjoiaHR0cHM6Ly9saDUuZ29vZ2xldXNlcmNvbnRlbnQuY29tLy1MSXBEbE1Bb19hcy9BQUFBQUFBQUFBSS9BQUFBQUFBQUFBQS9BR2k0Z2Z4ZmlhTFo3OEdZYUJIZTY0djBmMlBndElXRDZnL3M5Ni1jL3Bob3RvLmpwZyIsImdpdmVuX25hbWUiOiJMZW8iLCJmYW1pbHlfbmFtZSI6IlZpdGFsIiwibG9jYWxlIjoicHQifQ.P4x8zyDn3jE5MUqvetVVTQOKj_GUIvL1leRxxgZ-_NPWSgA3dB6BRJNPLKwlMXTKtBThD5iUmDiJ6NqdjMI5WVbkBHmVeh5ZmnzqO-9RGq_Di72fSiz60p4jMzDOVmvcfnzcLVob0S3flKWSwflxGz3Wkce2-yyeJX1HEMQYPtO6f8I_-w43cgnL8J1dJPpOhbxF-m9RYUuI6Xhnk_ZeOMZr9q1CZa5tJmsepfYsRikluNXLaMzMvN0TcQvunbjR3ZXTY10Sw6cv4CKCi_CsIMdKlDRu1GodmlLelx6lg26s9j2PFzwOCgzBWf8fXHcISN0X5S_0pPxrDHjA_3lytA"
    h <- liftIO $ return (either (\_ -> error "Error decoding JWT!") (decode . BS.fromStrict) (BS64.decode $ C8.pack $ getHeaderWithPadding jwt) :: Maybe JWTHeader)
    k <- return $ getKidFromHeader h
    r <- WREQ.get "https://www.googleapis.com/oauth2/v1/certs"
    certificate <- return $ r ^? WREQ.responseBody . DAL.key (T.pack k) . DAL._String
    case certificate of
        Just cert -> do
            case decodedSig of
                Right bytes -> do
                    let ver = verify pubk jwtToken (BS.fromStrict bytes)
                    return ver
                Left _ -> return False
            where
                payloadSig = extractPayloadSig jwt
                rsaCertificate = fromString $ T.unpack cert
                jwtToken = fromString $ payload payloadSig
                jwtSignature = fromString $ signature payloadSig
                signed = readSignedCertificateFromMemory rsaCertificate
                pubk = extractPubKey signed
                decodedSig = BS64.decode $ BS.toStrict jwtSignature
        Nothing -> return False