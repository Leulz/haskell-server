{-# LANGUAGE OverloadedStrings #-}
module ReadXLSX
    (containsMatricula
    ) where

import Codec.Xlsx
import qualified Data.ByteString.Lazy as L
import Control.Lens
import Data.Typeable()
import Data.Text as T

fileName :: String
fileName = "frequencia_2017.2_1411302-01_080207235.xlsx"
nomePlanilha :: Text
nomePlanilha = "Frequencia"
linhaDefault :: Int
linhaDefault = 3

-- valor inteiro começa de 1
capturaMatriculas :: IO([Maybe(String)])
capturaMatriculas = verifica linhaDefault []

-- linha começa de 1
verifica :: Int -> [Maybe(String)] -> IO([Maybe(String)])
verifica linha lista = do
    bs <- L.readFile fileName
    let value = (toXlsx bs ^? ixSheet nomePlanilha . ixCell (linha,2) . cellValue . _Just)

    if((convertCellValueToString value) == Nothing) then return lista else 
        verifica (linha+1) (lista++[convertCellValueToString value])

getCellValue :: CellValue -> String
getCellValue (CellText a) = T.unpack(a)

convertCellValueToString:: Maybe(CellValue) -> Maybe(String)
convertCellValueToString input = fmap (getCellValue) (input)

-- Dada uma matrícula retorna True se ela existir no conjunto de
-- matrículas lidos do arquivlo XLSX
containsMatricula:: String -> IO Bool
containsMatricula matricula = do
  matriculas <- capturaMatriculas
  return $ (Prelude.any (Just matricula==) matriculas)