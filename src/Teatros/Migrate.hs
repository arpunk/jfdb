{-# LANGUAGE OverloadedStrings #-}
module Teatros.Migrate where

import           Teatros.Persistent
import           Teatros.Parse
import           Teatros.Types
import           Teatros.Default

import           Control.Monad (forM_, mapM)
import           Control.Monad.Logger
import           Control.Monad.Trans.Resource

import           Data.Maybe

import           Data.Conduit
import           Data.Text.Encoding (decodeUtf8)

import           Database.Persist
import           Database.Persist.Sql
import           Database.Persist.Postgresql
import           Database.Persist.TH

import qualified Data.Text as T

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lex.Double as BS

import qualified Data.CSV.Conduit as CSV
import qualified Data.CSV.Conduit.Parser.Text as CSV

-- | Conexión a la DB
connStr = "dbname=RedTeatros2 host=localhost user=arpunk password='' port=5432"

runDb :: SqlPersistT (ResourceT (NoLoggingT IO)) a -> IO a
runDb query = runNoLoggingT $ runResourceT . withPostgresqlConn connStr . runSqlConn . (runMigration migrateAll >>) $ query

-- | Leer CSV
myCsvSettings :: CSV.CSVSettings
myCsvSettings = CSV.defCSVSettings { CSV.csvSep = '@' }

leerArchivo :: String -> IO BS.ByteString
leerArchivo = BS.readFile . (++) "data/"

decodificarCsv :: String -> Entidad -> IO ()
decodificarCsv archivo entidad = do
  dump <- leerArchivo archivo
  case CSV.parseCSV myCsvSettings (decodeUtf8 dump) of
    Left err -> putStrLn err
    Right v -> migrar entidad v

-- | Construcción de tipos de documentos

registrarEncabezamientos enc = mapM (\e -> do
  menc <- getBy $ UniqueEncabezamiento e
  case menc of
    Just (Entity eid _) -> return eid
    Nothing -> fail $ "No existe el encabezamiento " ++ T.unpack e) (encabezamientos enc)

migrar :: Entidad -> [[T.Text]] -> IO ()
migrar EntidadPeriodico es = runDb $
  mapM_ (\(sed:nom:tit:idi:lug:ano:mes:dia:pag:res:aut:nots:ufi:enc:uco:rdi:_) -> do
    sede <- traerSede sed
    encs <- registrarEncabezamientos enc

    ubicacion <- insert $ Ubicacion ufi uco rdi
    fecha     <- insert $ Fecha (Just $ parseInt $ stripSpace ano)
                                (Just $ parseInt $ stripSpace mes)
                                (Just $ parseInt $ stripSpace dia)
    ficha     <- insert $ FichaTecnica ubicacion (Just encs) fecha sede (Just nots) (Just lug)
    periodico <- insertUnique $ Periodico ficha tit nom idi pag res aut

    return ()) es

migrar EntidadProgramaMano es = runDb $
  forM_ es (\(sed:obr:aut:res:dirob:ano:mes:dia:lug:nots:ufi:enc:uco:rdi:_) -> do
    sede <- traerSede sed
    encs <- registrarEncabezamientos enc

    ubicacion    <- insert $ Ubicacion ufi uco rdi
    fecha        <- insert $ Fecha (Just $ parseInt ano) (Just $ parseInt mes) (Just $ parseInt dia)
    ficha        <- insert $ FichaTecnica ubicacion (Just encs) fecha sede (Just nots) (Just lug)
    programaMano <- insertUnique $ ProgramaMano ficha obr dirob res aut

    return ())

migrar EntidadAfiche es = runDb $
  forM_ es (\(sed:tit:agr:faf:lug:ano:mes:dia:disn:nots:ufi:enc:uco:rdi:_) -> do
    sede <- traerSede sed
    encs <- registrarEncabezamientos enc

    form <- traerFormatoAfiche faf

    ubicacion <- insert $ Ubicacion ufi uco rdi
    fecha     <- insert $ Fecha (Just $ parseInt ano) (Just $ parseInt mes) (Just $ parseInt dia)
    ficha     <- insert $ FichaTecnica ubicacion (Just encs) fecha sede (Just nots) (Just lug)
    afiche    <- insertUnique $ Afiche ficha tit form agr disn

    return ())

migrar EntidadFotografia es = runDb $
  forM_ es (\(sed:fot:eve:escen:disVest:tecn:faf:ano:mes:dia:lug:nots:ufi:enc:uco:rdi:_) -> do
    sede <- traerSede sed
    encs <- registrarEncabezamientos enc

    form <- traerFormatoFoto faf
    tec  <- traerTecnologiaFoto tecn

    ubicacion  <- insert $ Ubicacion ufi uco rdi
    fecha      <- insert $ Fecha (Just $ parseInt ano) (Just $ parseInt mes) (Just $ parseInt dia)
    ficha      <- insert $ FichaTecnica ubicacion (Just encs) fecha sede (Just nots) (Just lug)
    fotografia <- insertUnique $ Fotografia ficha form tec fot eve disVest escen

    return ())

migrar EntidadAudiovisual es = runDb $
  forM_ es (\(sed:tit:tecn:edi:tiem:ano:mes:dia:lug:nots:ufi:enc:uco:rdi:_) -> do
    sede <- traerSede sed
    encs <- registrarEncabezamientos enc

    tec  <- traerTecnologiaAV tecn

    ubicacion   <- insert $ Ubicacion ufi uco rdi
    fecha       <- insert $ Fecha (Just $ parseInt ano) (Just $ parseInt mes) (Just $ parseInt dia)
    ficha       <- insert $ FichaTecnica ubicacion (Just encs) fecha sede (Just nots) (Just lug)
    audiovisual <- insertUnique $ Audiovisual ficha tec tit edi tiem

    return ())

migrar EntidadBibliografia es = runDb $
  forM_ es (\(sed:tit:aut:lug:edit:ano:mes:dia:pag:ufi:nots:tipDocu:enc:uco:rdi:_) -> do
    sede <- traerSede sed
    encs <- registrarEncabezamientos enc

    tdoc <- traerTipoDoc tipDocu

    ubicacion    <- insert $ Ubicacion ufi uco rdi
    fecha        <- insert $ Fecha (Just $ parseInt ano) (Just $ parseInt mes) (Just $ parseInt dia)
    ficha        <- insert $ FichaTecnica ubicacion (Just encs) fecha sede (Just nots) (Just lug)
    bibliografia <- insertUnique $ Bibliografia ficha tdoc tit edit (parseInt pag) aut

    return ())

migrar EntidadPremio es = runDb $
  forM_ es (\(sed:tit:inst:lug:ano:mes:dia:tecn:nots:ufi:enc:uco:rdi:_) -> do
    sede <- traerSede sed
    encs <- registrarEncabezamientos enc

    ubicacion <- insert $ Ubicacion ufi uco rdi
    fecha     <- insert $ Fecha (Just $ parseInt ano) (Just $ parseInt mes) (Just $ parseInt dia)
    ficha     <- insert $ FichaTecnica ubicacion (Just encs) fecha sede (Just nots) (Just lug)
    premio    <- insertUnique $ Premio ficha tit inst tecn

    return ())

migrar EntidadObraGrafica es = runDb $
  forM_ es (\(sed:tit:forma:disVes:esce:ano:mes:dia:lug:autor:tecn:nots:ufi:uco:rdi:_) -> do
    sede <- traerSede sed
    form <- traerFormatoAfiche forma

    ubicacion <- insert $ Ubicacion ufi uco rdi
    fecha     <- insert $ Fecha (Just $ parseInt ano) (Just $ parseInt mes) (Just $ parseInt dia)
    ficha     <- insert $ FichaTecnica ubicacion Nothing fecha sede (Just nots) (Just lug)
    obra      <- insertUnique $ ObraGrafica ficha tit form disVes esce autor tecn

    return ())

migrar EntidadActividadCultural es = runDb $
  forM_ es (\(sed:agru:ano:mes:dia:nots:ufi:uco:rdi:_) -> do
    sede <- traerSede sed

    ubicacion <- insert $ Ubicacion ufi uco rdi
    fecha     <- insert $ Fecha (Just $ parseInt ano) (Just $ parseInt mes) (Just $ parseInt dia)
    ficha     <- insert $ FichaTecnica ubicacion Nothing fecha sede (Just nots) (Just "Ningúno")
    actividad <- insertUnique $ ActividadCultural ficha agru

    return ())

migrar EntidadProgramaAcademico es = runDb $
  forM_ es (\(sed:tit:prog:profs:ano:mes:lug:uco:rdi:_) -> do
    sede <- traerSede sed

    ubicacion <- insert $ Ubicacion "Ningúna" uco rdi
    fecha     <- insert $ Fecha (Just $ parseInt ano) (Just $ parseInt mes) Nothing
    ficha     <- insert $ FichaTecnica ubicacion Nothing fecha sede Nothing (Just lug)
    actividad <- insertUnique $ ProgramaAcademico ficha tit profs prog

    return ())
