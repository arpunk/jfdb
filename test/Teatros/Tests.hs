module Teatros.Tests where

import Test.HUnit hiding (Test)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework (Test, testGroup)

-- | Grupo principal de pruebas
tests :: Test
tests = testGroup "jfdb"
      [ testCase "Crear valores iniciales en la DB" jfdbCreateInitialDbTest
      , testCase "Leer archivos CSV" jfdbReadCsvFilesTest
      , testCase "Convertir filas a entidades" jfdbConvertRowToEntityTest
      , testCase "Crear las relaciones por entidad" jfdbCreateAndStoreRelationsTest
      ]

-- | Insertar casos de prueba
jfdbCreateInitialDbTest = return ()

jfdbReadCsvFilesTest = return ()

jfdbConvertRowToEntityTest = return ()

jfdbCreateAndStoreRelationsTest = return ()
