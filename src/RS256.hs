{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE RankNTypes #-}

module RS256 where

import Import.NoFoundation

import Data.String (fromString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Base64.URL as BS64

import Data.X509 (SignedCertificate, PubKey(PubKeyRSA), certPubKey, getCertificate)
import Data.X509.Memory (readSignedObjectFromMemory)

import Codec.Crypto.RSA (verify)
import qualified Crypto.Types.PubKey.RSA as RSA (PublicKey(PublicKey))
import Crypto.PubKey.RSA.Types (public_size, public_n, public_e)

import qualified Network.Wreq as WREQ
import Control.Lens
import qualified Data.Aeson.Lens as DAL (_String, key)
import Data.Map as Map
import Data.Aeson (Value)

type Resp = Response (Map String Value)

readSignedCertificateFromMemory :: B.ByteString -> [SignedCertificate]
readSignedCertificateFromMemory = readSignedObjectFromMemory

extractRSA (PubKeyRSA rsaPubKey) = RSA.PublicKey (public_size rsaPubKey) (public_n rsaPubKey) (public_e rsaPubKey)
extractRSA _ = error "Error!"

extractPubKey (x:[]) = extractRSA $ certPubKey $  getCertificate x
extractPubKey _ = error "Error!"

testRS256 :: IO (Bool)
testRS256 = do
    r <- WREQ.get "https://www.googleapis.com/oauth2/v1/certs"
    certificate <- return $ r ^? WREQ.responseBody . DAL.key "c6f0ee16be70c488dd39db70f6f4d137a04891e3" . DAL._String
    case certificate of
        Just cert -> do
            let rsaCertificate = fromString $ unpack cert
            let jwtToken = fromString $ "eyJhbGciOiJSUzI1NiIsImtpZCI6ImM2ZjBlZTE2YmU3MGM0ODhkZDM5ZGI3MGY2ZjRkMTM3YTA0ODkxZTMifQ.eyJhenAiOiI1ODI0MzczNzE4NDctdHJza2hubWdzaXFtcmZyZGJ1cmxlZG44anVvMWVzc3QuYXBwcy5nb29nbGV1c2VyY29udGVudC5jb20iLCJhdWQiOiI1ODI0MzczNzE4NDctdHJza2hubWdzaXFtcmZyZGJ1cmxlZG44anVvMWVzc3QuYXBwcy5nb29nbGV1c2VyY29udGVudC5jb20iLCJzdWIiOiIxMDYzMjAxODYzMDAzNDQ1ODcyNzEiLCJoZCI6ImNjYy51ZmNnLmVkdS5iciIsImVtYWlsIjoibGVvLnZpdGFsQGNjYy51ZmNnLmVkdS5iciIsImVtYWlsX3ZlcmlmaWVkIjp0cnVlLCJhdF9oYXNoIjoibGxKRGFlcHVVYlFteVBwWS1lbnZQUSIsImV4cCI6MTUyMTYwMDQ2MywiaXNzIjoiYWNjb3VudHMuZ29vZ2xlLmNvbSIsImp0aSI6IjhmMTFkNDU5MTgwZmI3NTExY2UwNDc2Zjc4NmFlNDU1YzJjMTVjOTAiLCJpYXQiOjE1MjE1OTY4NjMsIm5hbWUiOiJMZW8gVml0YWwiLCJwaWN0dXJlIjoiaHR0cHM6Ly9saDUuZ29vZ2xldXNlcmNvbnRlbnQuY29tLy1MSXBEbE1Bb19hcy9BQUFBQUFBQUFBSS9BQUFBQUFBQUFBQS9BR2k0Z2Z4ZmlhTFo3OEdZYUJIZTY0djBmMlBndElXRDZnL3M5Ni1jL3Bob3RvLmpwZyIsImdpdmVuX25hbWUiOiJMZW8iLCJmYW1pbHlfbmFtZSI6IlZpdGFsIiwibG9jYWxlIjoicHQifQ"
            let jwtSignature = fromString $ "P4x8zyDn3jE5MUqvetVVTQOKj_GUIvL1leRxxgZ-_NPWSgA3dB6BRJNPLKwlMXTKtBThD5iUmDiJ6NqdjMI5WVbkBHmVeh5ZmnzqO-9RGq_Di72fSiz60p4jMzDOVmvcfnzcLVob0S3flKWSwflxGz3Wkce2-yyeJX1HEMQYPtO6f8I_-w43cgnL8J1dJPpOhbxF-m9RYUuI6Xhnk_ZeOMZr9q1CZa5tJmsepfYsRikluNXLaMzMvN0TcQvunbjR3ZXTY10Sw6cv4CKCi_CsIMdKlDRu1GodmlLelx6lg26s9j2PFzwOCgzBWf8fXHcISN0X5S_0pPxrDHjA_3lytA==" --Add padding with ==
            let signed = readSignedCertificateFromMemory rsaCertificate
            let pubk = extractPubKey signed
            let decodedSig = BS64.decode $ toStrict jwtSignature
            case decodedSig of
                Right bytes -> do
                    let ver = verify pubk jwtToken (fromStrict bytes)
                    return ver
                Left _ -> return False
        Nothing -> return False