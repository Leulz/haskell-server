{-# LANGUAGE OverloadedStrings #-}
module ReadXLSX
    ( capturaMatriculas
    ) where

import Codec.Xlsx
import qualified Data.ByteString.Lazy as L
import Control.Lens
import Data.Typeable
import Data.Text as T

fileName = "frequencia_2017.2_1411302-01_080207235.xlsx"
nomePlanilha = "Frequencia"

-- valor inteiro começa de 1
capturaMatriculas :: IO([Maybe(String)])
capturaMatriculas = verifica 3 []

-- linha começa de 1
verifica :: Int -> [Maybe(String)] -> IO([Maybe(String)])
verifica linha lista = do
	bs <- L.readFile fileName
	let value = (toXlsx bs ^? ixSheet nomePlanilha .
              ixCell (linha,2) . cellValue . _Just)

	if((convertCellValueToString value) == Nothing) then return lista else
			verifica (linha+1) (lista++[convertCellValueToString value])

getCellValue :: CellValue -> String
getCellValue (CellText a) = T.unpack(a)

convertCellValueToString:: Maybe(CellValue) -> Maybe(String)
convertCellValueToString input = fmap (getCellValue) (input)


-- Trecho de código morto, porém util
-- funcao de teste não compartilhada nem utilizada no modulo
someFunc :: IO()
someFunc = do
	bs <- L.readFile fileName
	putStrLn $ show (typeOf bs)
	let value = toXlsx bs ^? ixSheet nomePlanilha .
              ixCell (3,2) . cellValue . _Just

  --putStrLn $ show (typeOf bs)
  	putStrLn $ "Cell B3 contains " ++ show (removingMaybeType value)
 -- putStrLn $ show (otherFunc (value) == "117210300")

removingMaybeType :: Maybe (CellValue) -> String
removingMaybeType (Just a) = getCellValue a
removingMaybeType Nothing = "Nothing"