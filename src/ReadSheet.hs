{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module ReadSheet (
	containsMatricula,
	getFuncaoPorMatricula) where 

---------------------------------------------------------------------------------
import Network.Google.Resource.Sheets.Spreadsheets.Get
import Network.Google.Sheets
import Network.Google
import Network.Google.Sheets.Types

import Control.Lens           ((.~), (<&>), (^.), view, makeLenses)
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

data PessoaAdministrador = PessoaAdministrador
		{ _matricula :: String, 
		 _nome :: String,
		 _email :: String, 
		 _funcao :: String} deriving(Eq, Show)
makeLenses ''PessoaAdministrador

sheetId = "1N755Sj0TN9DtAme3T4-EoPCHBcnehivfrd0xU6J97yQ"
range = "Página1!A:D"

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

coletarDadosSheet :: IO([PessoaAdministrador])
coletarDadosSheet = coletarDados sheetId range

containsMatricula matricula = do
	listaMatricula <- (filtraPessoas matricula)
	if( listaMatricula == []) then return False else return True 

filtraPessoas matricula = do 
	pessoas <- coletarDadosSheet
	return (Prelude.filter(\pessoa -> (getMatricula pessoa)== matricula) pessoas)

getFuncaoPorMatricula matricula = do
	listaPessoa <- (filtraPessoas matricula)
	return (getFuncao (head listaPessoa))

coletarDados :: String -> String -> IO([PessoaAdministrador])
coletarDados sheetID range = do
	valueRange <- exampleGetValue (pack(sheetID)) (pack(range))
	--putStrLn $ show (valueRange)
	--putStrLn $ show (typeOf (valueRange))
	--putStrLn $ show (coletaPessoasDeMatriz (valueRange ^. vrValues))
	return (coletaPessoasDeMatriz (valueRange^.vrValues))

coletaPessoasDeMatriz :: [[Value]] -> [PessoaAdministrador]
coletaPessoasDeMatriz [] = []
coletaPessoasDeMatriz lista = [getPessoa $ head lista]++(coletaPessoasDeMatriz (tail lista))

getPessoa :: [Value] -> PessoaAdministrador
getPessoa lista = PessoaAdministrador { _matricula = matricula, _nome = nome, _email = email, _funcao = funcao }
	where
		matricula = getMatriculaFromList lista
		nome = getNomeFromList lista
		email = getEmailFromList lista
		funcao = getFuncaoFromList lista

getMatriculaFromList lista = removeValue (lista !! 0)
getNomeFromList lista = removeValue (lista !! 1)
getEmailFromList lista = removeValue (lista !! 2)
getFuncaoFromList lista = removeValue (lista !! 3)

removeValue :: Value -> String
removeValue (String a) = unpack(a)

getNome :: PessoaAdministrador-> String
getNome pessoa = pessoa^.nome

getMatricula :: PessoaAdministrador-> String
getMatricula pessoa = pessoa^.matricula

getEmail :: PessoaAdministrador-> String
getEmail pessoa = pessoa^.email

getFuncao :: PessoaAdministrador-> String
getFuncao pessoa = pessoa^.funcao