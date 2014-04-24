{-# LANGUAGE EmptyDataDecls, FlexibleContexts, GADTs, OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies #-}
module Teatros.Persistent where

import           Database.Persist.TH

import           Data.Text (Text)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
FichaTecnica
  ubicacion       UbicacionId
  encabezamientos [EncabezamientoId] Maybe
  fecha           FechaId
  sede            SedeId
  notas           Text Maybe
  lugar           Text Maybe
  UniqueFichaTecnica ubicacion fecha sede

Fecha
  ano Int Maybe
  mes Int Maybe
  dia Int Maybe

Sede
  nombre Text
  UniqueSede nombre

Encabezamiento
  nombre Text
  UniqueEncabezamiento nombre

Ubicacion
  fisica  Text
  copias  Text
  digital Text

Periodico
  fichaTecnica FichaTecnicaId
  titulo       Text
  nombre       Text
  idioma       Text
  pagina       Text
  resumen      Text
  autor        Text
  UniquePeriodico fichaTecnica nombre

ProgramaMano
  fichaTecnica FichaTecnicaId
  obra         Text
  directorObra Text
  resumen      Text
  autor        Text
  UniqueProgramaMano fichaTecnica obra

Afiche
  fichaTecnica FichaTecnicaId
  titulo       Text
  formato      FormatoAficheId
  agrupacion   Text
  disenador    Text
  UniqueAfiche fichaTecnica titulo

Fotografia
  fichaTecnica FichaTecnicaId
  formato      FormatoFotografiaId
  tecnologia   TecnologiaFotografiaId
  fotografo    Text
  evento       Text
  disenadorV   Text
  escenografo  Text
  UniqueFotografia fichaTecnica fotografo

Audiovisual
  fichaTecnica FichaTecnicaId
  tecnologia   TecnologiaAudiovisualId
  titulo       Text
  edicion      Text
  tiempo       Text
  UniqueAudiovisual fichaTecnica titulo

Bibliografia
  fichaTecnica FichaTecnicaId
  tipoDoc      TipoDocumentoId
  titulo       Text
  editorial    Text
  paginas      Int
  autor        Text
  UniqueBibliografia fichaTecnica titulo

Premio
  fichaTecnica FichaTecnicaId
  titulo       Text
  institucionO Text
  tecnica      Text
  UniquePremio fichaTecnica titulo

ObraGrafica
  fichaTecnica FichaTecnicaId
  titulo       Text
  formato      FormatoAficheId
  disenadorV   Text
  escenografo  Text
  autor        Text
  tecnica      Text
  UniqueObraGrafica fichaTecnica titulo

ActividadCultural
  fichaTecnica FichaTecnicaId
  agrupaciones Text
  UniqueActividadCultural fichaTecnica

ProgramaAcademico
  fichaTecnica FichaTecnicaId
  titulo       Text
  profesores   Text
  programa     Text
  UniqueProgramaAcademico fichaTecnica titulo

TecnologiaFotografia
  nombre Text
  UniqueTecnologiaFotografia nombre

TecnologiaAudiovisual
  nombre Text
  UniqueTecnologiaAudiovisual nombre

FormatoFotografia
  nombre Text
  UniqueFormatoFotografia nombre

FormatoAfiche
  nombre Text
  UniqueFormatoAfiche nombre

TipoDocumento
  nombre Text
  UniqueTipoDocumento nombre
|]
