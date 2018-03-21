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
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Base64.URL as BS64
import Data.String (fromString)
import Data.X509 (SignedCertificate, PubKey(PubKeyRSA), certPubKey, getCertificate)
import Data.X509.Memory (readSignedObjectFromMemory)
import Codec.Crypto.RSA (verify)

import qualified Crypto.Types.PubKey.RSA as RSA (public_size, PublicKey(PublicKey))
import Crypto.PubKey.RSA.Types (public_size, public_n, public_e)

readSignedCertificateFromMemory :: B.ByteString -> [SignedCertificate]
readSignedCertificateFromMemory = readSignedObjectFromMemory

extractRSA (PubKeyRSA rsaPubKey) = RSA.PublicKey (public_size rsaPubKey) (public_n rsaPubKey) (public_e rsaPubKey)
extractRSA _ = error "Error!"

extractPubKey (x:[]) = extractRSA $ certPubKey $  getCertificate x
extractPubKey _ = error "Error!"

testRS256 :: IO (Bool)
testRS256 = do
    let rsaCertificate = fromString $ "-----BEGIN CERTIFICATE-----\nMIIDJjCCAg6gAwIBAgIISAVeUtILmmAwDQYJKoZIhvcNAQEFBQAwNjE0MDIGA1UE\nAxMrZmVkZXJhdGVkLXNpZ25vbi5zeXN0ZW0uZ3NlcnZpY2VhY2NvdW50LmNvbTAe\nFw0xODAzMTQxNDQ5MDhaFw0xODAzMzEwMzA0MDhaMDYxNDAyBgNVBAMTK2ZlZGVy\nYXRlZC1zaWdub24uc3lzdGVtLmdzZXJ2aWNlYWNjb3VudC5jb20wggEiMA0GCSqG\nSIb3DQEBAQUAA4IBDwAwggEKAoIBAQCONoQCtYfwFBLBjQo0z+6NLYe1FiRZhlKK\n1R0igt5u9tiazTFc/vxxYZtRFYoa9Po4UpfrmiAqPKQRUKwJOEphaa+u4H3R06t9\nPA31qPJ17djKcmo083sF8pQlAKPKgMPCfNZFfR6AattQYCCRINyS3aeAvRtbizKY\n8LDaks/YcwNXkqv7uAoen2CzLcYA1asLgXFmcPmBtoBQMbPO77DJ6H9jpYpdZVFb\nhP/zbWYr0ZILFpLZwFv7m7FHSl4U19bhLsIYXKunx3oMLgla0KAkk8RV5bQqs09M\n5XukbuXfxInpZKAv4av93GW6h34F6k7rydTIEvDsVdP58cw3e8ihAgMBAAGjODA2\nMAwGA1UdEwEB/wQCMAAwDgYDVR0PAQH/BAQDAgeAMBYGA1UdJQEB/wQMMAoGCCsG\nAQUFBwMCMA0GCSqGSIb3DQEBBQUAA4IBAQAZkWOojZ7AI2M3xXcwWKYvaLZQCWib\nOMcibj21tjCiJlmRpB5YOWKkIhSG7PY7tizEcnPjTEKZNczhwHAIm3XBZUSXRgl7\n2SWSFAsEdkvW/IkP/UdAS7sF7qw/l7CEfp0Dpp7lqSne/xfsWJB05wz+A3vb/igK\nKGuHh/uAKquF8QhvEpoJXD9DySHST/fJ0BEK8nFSI3S5jiriXGjESV92jHI/5XFA\nGSwW8+ExiCL0h1yK05vWpHr/kzbvcBf3pEm8/ERE3rJO4IYKe7fBaLCGTWrCSpm3\ngpZ/HRxT0kKvTG+V7pdDxqZ8aoJ2ZXEGfvcvEC7R9anT4h38gcnwnOUk\n-----END CERTIFICATE-----\n"
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