{-# LANGUAGE OverloadedStrings #-}
module ReadXLSX
    ( verificaMat
    ) where

import Codec.Xlsx
import qualified Data.ByteString.Lazy as L
import Control.Lens
import Data.Typeable
import Data.Text as T

fileName = "frequencia_2017.2_1411302-01_080207235.xlsx"
nomePlanilha = "Frequencia"

-- valor inteiro começa de 1
verificaMat :: String -> IO(Bool)
verificaMat matricula = verifica matricula 2

-- linha começa de 1
verifica :: String -> Int -> IO(Bool)
verifica matricula linha = do
	bs <- L.readFile fileName
	let value = toXlsx bs ^? ixSheet nomePlanilha .
              ixCell (linha,2) . cellValue . _Just

	if(removingMaybeType value == "Nothing") then return False else
		if(removingMaybeType value == matricula) then return True else
			(verifica matricula (linha+1))

removingMaybeType :: Maybe (CellValue) -> String
removingMaybeType (Just a) = getCellValue a
removingMaybeType Nothing = "Nothing"

getCellValue :: CellValue -> String
getCellValue (CellText a) = T.unpack(a)


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