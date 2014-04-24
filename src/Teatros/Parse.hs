{-# LANGUAGE TypeFamilies, NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings, GADTs, FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Teatros.Parse where

import           Teatros.Persistent

import           Database.Persist
import           Control.Applicative ((<$>))

import           Data.Char (isSpace)
import           Data.List.Split

import qualified Data.Text as T

encabezamientos :: T.Text -> [T.Text]
encabezamientos x = T.pack <$> split (startsWithOneOf ['A' .. 'Z']) (T.unpack x)

stripSpace :: T.Text -> T.Text
stripSpace = T.dropWhile isSpace

parseInt :: T.Text -> Int
parseInt x
  | T.length x > 0 = (read . T.unpack) x
  | otherwise = 0

registrarEncabezamiento recurso enc' tabla = do
  enc <- getBy $ UniqueEncabezamiento enc'
  case enc of
    Just e -> insertUnique $ tabla recurso (entityKey e)
    Nothing -> fail $ "No existe el encabezamiento " ++ T.unpack enc'

traerEntidad filtro entidad = do
  f <- selectFirst [ filtro ==. entidad ] []
  case f of
    Just (Entity eid _) -> return eid
    Nothing -> fail $ "No existe la entidad " ++ T.unpack entidad

traerEntidad' filtro formato alt = do
  f <- selectFirst [ filtro ==. formato ] []
  case f of
    Just (Entity eid _) -> return eid
    Nothing -> do
      otro <- selectFirst [ filtro ==. alt ] []
      case otro of
        Just (Entity oid _) -> return oid
        Nothing -> fail $ "Imposible guardar el formato " ++ T.unpack alt

traerSede    = traerEntidad SedeNombre
traerTipoDoc = traerEntidad TipoDocumentoNombre

traerFormatoAfiche = traerEntidad' FormatoAficheNombre "Otro"
traerFormatoFoto   = traerEntidad' FormatoFotografiaNombre "Otro"

traerTecnologiaFoto = traerEntidad' TecnologiaFotografiaNombre "Sin definir"
traerTecnologiaAV   = traerEntidad' TecnologiaAudiovisualNombre "Sin definir"
