{-# LANGUAGE OverloadedStrings #-}
module Teatros.Migrate where

import qualified Data.Text as T
import qualified Data.ByteString as BS

-- | Leer CSV
leerArchivo :: String -> IO BS.ByteString
leerArchivo = BS.readFile . (++) "data/"

-- | Construcción de tipos de documentos
migrarPeriodico :: IO ()
migrarPeriodico = undefined

migrarProgramaMano :: IO ()
migrarProgramaMano = undefined

migrarAfiche :: IO ()
migrarAfiche = undefined

migrarFotografia :: IO ()
migrarFotografia = undefined

migrarAudiovisual :: IO ()
migrarAudiovisual = undefined

migrarBibliografia :: IO ()
migrarBibliografia = undefined

migrarPremio :: IO ()
migrarPremio = undefined

migrarObraGrafica :: IO ()
migrarObraGrafica = undefined

migrarActividadCultural :: IO ()
migrarActividadCultural = undefined
