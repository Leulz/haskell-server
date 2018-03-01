{-# LANGUAGE OverloadedStrings #-}

module ReadSheet (
	funcTeste) where 

---------------------------------------------------------------------------------
import Network.Google.Resource.Sheets.Spreadsheets.Get
import Network.Google.Sheets
import Network.Google
import Network.Google.Sheets.Types

import Control.Lens           ((.~), (<&>), (^.), view)
import Data.Text              (Text, pack, unpack)
import System.IO              (stdout)
import Data.Aeson.Types


import Data.Typeable
---------------------------------------------------------------------------------
-- | 
-- This gets the Information about an spreadsheet.
-- In order to be able to run these examples you need to
-- create a service acount from google's developers console
-- and copy the dowloaded json file to ~/.config/gcloud/application_default_credentials.json.
-- 
-- you must also share with your service the spreadsheet that you want to get the info of.
-- In order to do this you must share the sheet with the email address of your service
-- which is in your downloaded service config file.
--
-- after doing above step just pass the sreadsheet id to the function.
exampleGetSheet :: Text -> IO Spreadsheet
exampleGetSheet sheetID = do
  lgr <- newLogger Debug stdout
  env <- newEnv <&> (envLogger .~ lgr) . (envScopes .~ spreadsheetsScope)
  runResourceT . runGoogle env $
    send (spreadsheetsGet sheetID )

-- | exampleGetValue (pack("1N755Sj0TN9DtAme3T4-EoPCHBcnehivfrd0xU6J97yQ")) (pack("Página1!A:A"))
-- you pass the sheet id and a range (eg. "sheet1!A1:C3") in that sheet 
-- and it retreives the values in the specified range
exampleGetValue :: Text -> Text -> IO ValueRange 
exampleGetValue sheetID range = do
  lgr <- newLogger Debug stdout
  env <- newEnv <&> (envLogger .~ lgr) . (envScopes .~ spreadsheetsScope)
  runResourceT . runGoogle env $
    send  (spreadsheetsValuesGet sheetID range )

funcTeste :: String -> String -> IO([String])
funcTeste sheetID range = do
	valueRange <- exampleGetValue (pack(sheetID)) (pack(range))
	--putStrLn $ show (valueRange)
	--putStrLn $ show (typeOf (valueRange))
	return (imprime (valueRange^.vrValues))

imprime :: [[Value]] -> [String]
imprime [] = []
imprime lista = [imprimeL $ head lista]++(imprime (tail lista))

imprimeL :: [Value] -> String
imprimeL lista = (removeValue $ head lista)

removeValue :: Value -> String
removeValue (String a) = unpack(a)