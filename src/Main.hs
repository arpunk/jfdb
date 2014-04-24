{-# LANGUAGE OverloadedStrings #-}
module Main where

import Teatros.Migrate
import Teatros.Types

main :: IO ()
main = do
  decodificarCsv "periodicos.txt"             EntidadPeriodico
  decodificarCsv "programas-mano.txt"         EntidadProgramaMano
  decodificarCsv "afiches.txt"                EntidadAfiche
  decodificarCsv "fotografias.txt"            EntidadFotografia
  decodificarCsv "audiovisuales.txt"          EntidadAudiovisual
  decodificarCsv "bibliografias.txt"          EntidadBibliografia
  decodificarCsv "premios.txt"                EntidadPremio
  decodificarCsv "obras-graficas.txt"         EntidadObraGrafica
  decodificarCsv "actividades-culturales.txt" EntidadActividadCultural
  decodificarCsv "programas-academicos.txt"   EntidadProgramaAcademico
