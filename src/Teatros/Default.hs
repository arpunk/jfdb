{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies, DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings, GADTs, FlexibleContexts, EmptyDataDecls #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, NoMonomorphismRestriction #-}
module Teatros.Default (prepararDb) where

import           Teatros.Persistent

import           Database.Persist (insertUnique)
import           Control.Monad (forM_)

xsFormatoAfiches = ["Oficio (22x33)", "Tabloide (29x41)"
                   ,"Medio Pliego (50x70)", "Pliego (100x70)"
                   ,"Gran Formato (mayor a 100x70)", "Otro"]

xsFormatoFotos = ["Documento (5x4)", "Medio Oficio (22x16,5)"
                 ,"Oficio (22x33)", "Medio Pliego (70x50)"
                 ,"Otro"]

xsTecnologiasFotos = ["Análoga - BN", "Análoga - Color", "Digital - BN"
                     ,"Digital - Color", "Diapositiva - BN"
                     ,"Diapositiva-Color", "Sin definir"]

xsTecnologiasAudiovisuales = ["BETACAM", "BETA", "VHS", "DVD", "CD"
                             ,"Cassette", "Carrete Abierto", "Disco"
                             ,"Material Fílmico", "Otra"]

xsSedes = ["Casa Teatro Nacional", "Teatro La Castellana"
          ,"Teatro Fanny Mikey Calle 71"]

xsTiposDeDocumentos = ["Revista", "Libro", "Guión"]

xsEncabezamientos = ["Historia teatro", "Crítica teatral", "Festivales"
                    ,"Entrevistas", "Correspondencia"
                    ,"Programación cultural", "Fanny mikey", "Actores"
                    ,"Directores", "Dramaturgos"
                    ,"Proyecto pedagógico", "Trayectoria"]

importar t xs = forM_ xs $ (\e -> insertUnique $ t e)

prepararDb = do
  importar Sede                  xsSedes
  importar Encabezamiento        xsEncabezamientos
  importar FormatoAfiche         xsFormatoAfiches
  importar TecnologiaFotografia  xsTecnologiasFotos
  importar FormatoFotografia     xsFormatoFotos
  importar TipoDocumento         xsTiposDeDocumentos
  importar TecnologiaAudiovisual xsTecnologiasAudiovisuales
