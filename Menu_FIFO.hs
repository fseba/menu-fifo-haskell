------------------------------------------------------------------------------
-- Modulo      :  Menu_FIFO_Haskell
-- Programador :  Sebastián Uriel Flores
-- Estabilidad :  experimental
-- Portabilidad:  experimental
--
-- Programacion Avanzada en Haskell - Ing. Informática. 2018
-- 
-----------------------------------------------------------------------------    

{-# LANGUAGE OverloadedStrings #-}

module Menu_FIFO_Haskell where

--import System.Exit
import Data.Char
import Model.ModeloStock
import Model.QUEUEData
import System.IO
import System.Environment
import System.Directory
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy.Encoding as T


main:: IO()
main = do      
      --(provsFileName : itemsFileName : _) <- getArgs
      let provsFileName = "./Files/Proveedores.json"
      let itemsFileName = "./Files/Items.json"
      results <- retrieveAllData provsFileName itemsFileName

      putStrLn "---------------------------------------------------------"
      putStrLn "Se ha obtenido la siguiente información de los archivos: "
      putStrLn "---------------------------------------------------------"
      case results of 
            Just (proveedores,items) -> do
                  putStrLn $ showQ "Proveedores" proveedores
                  putStrLn ""
                  putStrLn $ showQ "Items" items
                  putStrLn ""
                  putStrLn "---------------------------------------------------------"                  
                  putStrLn "Presione ENTER para continuar"
                  putStrLn "---------------------------------------------------------"                  
                  getLine

                  --Now, show menu
                  loopMenu proveedores items provsFileName itemsFileName
            Nothing -> do 
                  putStrLn "No se pudieron cargar los archivos"
                  putStrLn "---------------------------------------------------------"

loopMenu :: ColaProveedores -> ColaItems -> FilePath -> FilePath -> IO()
loopMenu proveedores items proveedoresFileName itemsFileName = do
      putStrLn "¿Qué desea hacer?"
      putStrLn "- 1. Ver Proveedores"  
      putStrLn "- 2. Ver Items"  
      putStrLn "- 3. Agregar un Proveedor"
      putStrLn "- 4. Agregar un Item"
      putStrLn "- 5. Eliminar un Proveedor"    
      putStrLn "- 6. Eliminar un Item"          
      putStrLn "- 0. Salir"
      putStr "| --- > "
      userInput <- getLine

      if userInput == "0" 
            then return ()
      else do
            putStrLn ""            
            getOption userInput proveedores items proveedoresFileName itemsFileName        
            putStrLn ""
            
      
getOption :: String -> ColaProveedores -> ColaItems -> FilePath -> FilePath -> IO()
getOption opcion proveedores items proveedoresFileName itemsFileName = case opcion of
      "1" -> do
            showProveedores proveedores
            loopMenu proveedores items proveedoresFileName itemsFileName
      "2" -> do
            showItems items
            loopMenu proveedores items proveedoresFileName itemsFileName
      "3" -> do 
            proveedoresNuevo <- addProveedor proveedores proveedoresFileName
            loopMenu proveedoresNuevo items proveedoresFileName itemsFileName
      "4" -> do 
            itemsNuevo <- addItem items itemsFileName
            loopMenu proveedores itemsNuevo proveedoresFileName itemsFileName
      "5" -> do 
            proveedoresNuevo <- supressProveedor proveedores proveedoresFileName
            loopMenu proveedoresNuevo items proveedoresFileName itemsFileName
      "6" -> do 
            itemsNuevo <- supressItem items itemsFileName
            loopMenu proveedores itemsNuevo proveedoresFileName itemsFileName
      otherwise -> do 
            putStrLn "---------------------------------------------------------"
            putStrLn "Ha ingresado una opción incorrecta. Intente nuevamente"
            putStrLn "---------------------------------------------------------"            
            loopMenu proveedores items proveedoresFileName itemsFileName

-- Opción 1 del Menú
showProveedores :: ColaProveedores -> IO()
showProveedores proveedores = do
      putStrLn "---------------------------------------------------------"
      putStrLn "Proveedores del Sistema"
      putStrLn "---------------------------------------------------------"
      putStrLn $ showQ "Proveedores" proveedores      
      putStrLn "---------------------------------------------------------"                  

-- Opción 2 del Menú
showItems :: ColaItems -> IO()
showItems items = do
      putStrLn "---------------------------------------------------------"
      putStrLn "Items del Sistema"
      putStrLn "---------------------------------------------------------"
      putStrLn $ showQ "Items" items      
      putStrLn "---------------------------------------------------------"                  

-- Opción 3 del Menú
addProveedor :: ColaProveedores -> FilePath -> IO ColaProveedores
addProveedor proveedores fileName = do
      putStrLn "---------------------------------------------------------"
      putStrLn "Ingreso de Proveedor nuevo al sistema: "
      putStrLn "---------------------------------------------------------"
      
      -- Obtengo los datos del Proveedor
      codProveedor <- getLineUntil "*   Ingrese el código del proveedor -->   " :: IO Int      
      putStr "*   Ingrese el nombre del Proveedor -->   "
      nomProveedor <- getLine
      putStr "*   Ingrese la dirección del Proveedor -->   "
      dirProveedor <- getLine
      putStr "*   Ingrese el teléfono del Proveedor -->   "
      telProveedor <- getLine

      let proveedor = Proveedor {
            codigoProveedor = codProveedor,
            nombre = nomProveedor,
            direccion = dirProveedor,
            telefono = telProveedor
            }     

      -- Inserto el nuevo Proveedor en la Cola
      let proveedoresNuevo = insertQ proveedor proveedores
      
      --Reemplazo el archivo Proveedores por uno con la Cola modificada
      saveResult <- saveFile fileName $ encode (qtoL proveedoresNuevo)
      putStrLn "---------------------------------------------------------"

      case saveResult of 
            True -> do
                  putStrLn "El proveedor ha sido dado de alta exitosamente."
                  putStrLn "Estos son sus datos:"
                  putStrLn $ show proveedor
                  putStrLn "------------------------------------------------------------"
                  return proveedoresNuevo
            False -> do
                  putStrLn "Ha ocurrido un error al actualizar el archivo de proveedores."
                  putStrLn "El alta ha sido cancelada."                                    
                  putStrLn "------------------------------------------------------------"
                  return proveedores

-- Opción 4 del Menú
addItem :: ColaItems -> FilePath -> IO ColaItems
addItem items fileName = do
      putStrLn "---------------------------------------------------------"
      putStrLn "Ingreso de Item nuevo al sistema: "
      putStrLn "---------------------------------------------------------"
      
      -- Obtengo los datos del Item
      codItem <- getLineUntil "*   Ingrese el código del Item -->   " :: IO Int      
      putStr "*   Ingrese el nombre del Item -->   "
      nomItem <- getLine
      putStr "*   Ingrese la marca del Item -->   "
      marItem <- getLine
      putStr "*   Ingrese el rubro del Item -->   "
      rubItem <- getLine
      provItem <- getLineUntil "*   Ingrese el código del Proveedor del Item -->   " :: IO Int      
      putStr "*   Ingrese la unidad de medida del Item -->   "
      uMedItem <- getLine      
      cantExItem <- getLineUntil "*   Ingrese la cantidad existente del Item -->   " :: IO Int      
      vMinItem <- getLineUntil "*   Ingrese el valor mínimo del Item -->   " :: IO Int      
      vMaxItem <- getLineUntil "*   Ingrese el valor máximo del Item -->   " :: IO Int            
      pUnitItem <- getLineUntil "*   Ingrese el precio unitario del Item -->   " :: IO Float
      pGanItem <- getLineUntil "*   Ingrese el porcentaje de ganancia del Item -->   " :: IO Float

      let item = ItemStock {
            codigoItem = codItem,
            item = nomItem,
            marca = marItem,
            rubro = rubItem,
            proveedor = provItem,
            uMed = uMedItem,
            cantExistente = cantExItem,
            vMin = vMinItem,                  
            vMax = vMaxItem,
            precioU = pUnitItem,
            pGanancia = pGanItem
            }     

      let itemsNuevo = insertQ item items

      --Reemplazo el archivo Items por uno con la cola modificada
      saveResult <- saveFile fileName $ encode (qtoL itemsNuevo)
      putStrLn "---------------------------------------------------------"

      case saveResult of 
            True -> do
                  putStrLn "El Item ha sido dado de alta exitosamente."
                  putStrLn "Estos son sus datos:"
                  putStrLn $ show item
                  putStrLn "------------------------------------------------------------"
                  return itemsNuevo
            False -> do
                  putStrLn "Ha ocurrido un error al actualizar el archivo de Items."
                  putStrLn "El alta ha sido cancelada."                                    
                  putStrLn "------------------------------------------------------------"
                  return items

-- Opción 5 del Menú
supressProveedor :: ColaProveedores -> FilePath -> IO ColaProveedores
supressProveedor proveedores fileName = do
      putStrLn "---------------------------------------------------------"
      putStrLn "Eliminación de un Proveedor del sistema"
      putStrLn "---------------------------------------------------------"

      -- Obtengo el código de identificación del Proveedor a eliminar
      codProveedor <- getLineUntil "*   Ingrese el código del Proveedor -->   " :: IO Int
      putStrLn ""

      -- Filtro la Cola de Proveedores para quedarme con todos excepto aquel que quiero suprimir
      let proveedoresNuevo = filterQ (\x -> codigoProveedor x /= codProveedor) proveedores
      
      -- Si la Cola filtrada posee la misma longitud que la Cola original
      -- significa que no se encontró el Proveedor en la misma.
      -- Sino, la Cola nueva debe ser más corta,
      -- y significa que se encontró y pudo eliminarse.
      if cardQ proveedores /= cardQ proveedoresNuevo then do
            saveResult <- saveFile fileName $ encode (qtoL proveedoresNuevo)
            
            case saveResult of
                  True -> do
                        putStrLn "El Proveedor indicado se ha eliminado correctamente."
                        putStrLn "---------------------------------------------------------"
                        return proveedoresNuevo
                  False -> do
                        putStrLn "Ha ocurrido un error al actualizar el archivo de proveedores."
                        putStrLn "La baja ha sido cancelada."                                    
                        putStrLn "---------------------------------------------------------"
                        return proveedores
      else do
            putStrLn "No se encontró el código de Proveedor buscado."
            putStrLn "---------------------------------------------------------"
            return proveedores

-- Opción 6 del Menú
supressItem :: ColaItems -> FilePath -> IO ColaItems
supressItem items fileName = do
      putStrLn "---------------------------------------------------------"
      putStrLn "Eliminación de un Item del sistema"
      putStrLn "---------------------------------------------------------"
      
      -- Obtengo el código de identificación del Item a eliminar
      codItem <- getLineUntil "*   Ingrese el código del Item -->   " :: IO Int
      putStrLn ""

      -- Filtro la Cola de Items para qudarme con todos excepto aquel que quiero suprimir
      let itemsNuevo = filterQ (\x -> codigoItem x /= codItem) items
      
      -- Si la Cola filtrada posee la misma longitud que la Cola original
      -- significa que no se encontró el Item en la misma.
      -- Sino, la Cola nueva debe ser más corta,
      -- y significa que se encontró y pudo eliminarse.
      if cardQ items /= cardQ itemsNuevo then do
            saveResult <- saveFile fileName $ encode (qtoL itemsNuevo)
            putStrLn ""
            case saveResult of
                  True -> do                        
                        putStrLn "El Item indicado se ha eliminado correctamente."
                        putStrLn "---------------------------------------------------------"
                        return itemsNuevo
                  False -> do
                        putStrLn "Ha ocurrido un error al actualizar el archivo de Items."
                        putStrLn "La baja ha sido cancelada."                               
                        putStrLn "---------------------------------------------------------"     
                        return items
      else do
            putStrLn "No se encontró el código de Item buscado."
            putStrLn "---------------------------------------------------------"
            return items

-- Repite una instrucción *getLine* hasta que el Usuario ingrese
-- un dato del tipo *a*.
getLineUntil :: Read a => String -> IO a
getLineUntil guess = do
  putStr guess
  line <- getLine
  case (readMaybe line :: (Read a) => Maybe a) of
    Just x -> return x
    Nothing -> do
      putStrLn ""
      putStrLn "Debe ingresar un elemento del tipo correcto"
      putStrLn ""
      result <- getLineUntil guess
      return result

-- Devuelve *Just a* si lee un elemento de tipo *a*
-- caso contrario, devuelve *Nothing*
readMaybe :: Read a => String -> Maybe a
readMaybe s = case reads s of
      [(val, "")] -> Just val
      _           -> Nothing
      
-- Devuelve *Just* con los contenidos de un archivo, o Nothing en caso de error
loadFile :: FilePath -> IO (Maybe B.ByteString)
loadFile path = do 
      fileExists <- doesFileExist path
      case fileExists of
            True -> do
                  fileContents <- B.readFile path
                  return $ Just fileContents
            False -> return Nothing      

-- Devuelve *True* si logra guardar un archivo, o *False* en caso contrario
saveFile :: FilePath -> B.ByteString -> IO Bool
saveFile path contents = do 
      fileExists <- doesFileExist path
      case fileExists of
            True -> do
                  B.writeFile path contents
                  return $ True
            False -> return False

-- Devuelve dos Colas, una con los Proveedores y otra con los Items, 
-- ambos guardados en sus archivos correspondientes en disco
retrieveAllData :: FilePath -> FilePath -> IO (Maybe (ColaProveedores, ColaItems))
retrieveAllData provsFileName itemsFileName = do   
      proveedores <- retrieveProveedores provsFileName
      items <- retrieveItems itemsFileName

      case (proveedores,items) of
            (Nothing, _) -> return Nothing
            (_, Nothing) -> return Nothing
            (Just p, Just i) -> return $ Just(p,i)            
      
-- Devuelve una Cola con los Proveedores almacenados en un archivo en disco
retrieveProveedores :: FilePath -> IO (Maybe (ColaProveedores))
retrieveProveedores provsFileName = do
      provsRawContent <- loadFile provsFileName
      case provsRawContent of
            Just proveedores -> do
                  let provsList = decode $ proveedores :: Maybe [Proveedor]
                  case provsList of
                        Just [] -> return $ Just (EmptyQ :: ColaProveedores)
                        Just l -> return $ Just (Q l :: ColaProveedores)
                        Nothing -> return Nothing
            Nothing -> return Nothing

-- Devuelve una Cola con los Items almacenados en un archivo en disco
retrieveItems :: FilePath -> IO (Maybe (ColaItems))
retrieveItems itemsFileName = do
      itemsRawContent <- loadFile itemsFileName
      case itemsRawContent of
            Just items -> do
                  let itemsList = decode $ items :: Maybe [ItemStock]
                  case itemsList of
                        Just [] -> return $ Just (EmptyQ :: ColaItems)
                        Just l -> return $ Just (Q l :: ColaItems)
                        Nothing -> return Nothing
            Nothing -> return Nothing