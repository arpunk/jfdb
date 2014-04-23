{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies, DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings, GADTs, FlexibleContexts, EmptyDataDecls #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, NoMonomorphismRestriction #-}
module Teatros.Parse where

import           Teatros.Persistent

import           Database.Persist
import           Control.Monad (forM_)

import           Data.List.Split

import qualified Data.Text as T

encabezamientos :: T.Text -> [T.Text]
encabezamientos x = fmap T.pack $ split (startsWithOneOf ['A' .. 'Z']) (T.unpack x)

registrarEncabezamiento recurso enc' tabla = do
  enc <- getBy $ UniqueEncabezamiento enc'
  case enc of
    Just e -> do
      _ <- insertUnique $ tabla recurso (entityKey e)
      return ()
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
