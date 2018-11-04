------------------------------------------------------------------------------
-- Modulo      :  ModeloStock
-- Programador :  
-- Estabilidad :  experimental
-- Portabilidad:  experimental
--
-- Programacion Avanzada en Haskell - Ing. Informática. 2018
-- Este Modulo implementa Colas/Filas Finitas (FIFO)
-----------------------------------------------------------------------------         
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Model.ModeloStock where

import Model.QUEUEData
import Data.Aeson
import Data.Text
import GHC.Exts
import GHC.Generics

{--data QData a = Q [a]  deriving (Show,Eq,Ord)
--     deriving (Eq,Ord)


--data QDataR a = EmptyQR | QR {prim::a, resto::QDataR a} deriving (Show,Eq,Ord)
--     deriving (Eq,Ord)


--infixr 5 :-:
--data QDataOp a = EmptyQOp | a :-: (QDataOp a)  deriving (Show,Eq,Ord)
 --     deriving (Eq,Ord)
--___________________________________________________________________________________________

-- MODELO SISTEMA DE STOCK DE ALMACEN
--___________________________________________________________________________________________
--}

data ItemStock = ItemStock {
   codigoItem :: Int,
   item :: String,
   marca :: String,
   rubro :: String,
   proveedor :: Int,
   uMed :: String,
   cantExistente :: Int,
   vMin :: Int,
   vMax :: Int,
   precioU ::Float,
   pGanancia :: Float
  } deriving (Show, Eq)

data Proveedor = Proveedor {
   codigoProveedor :: Int,
   nombre :: String,
   direccion :: String,
   telefono :: String
  } deriving (Show, Eq)

type ColaItems = QData ItemStock -- Cola de Items
type ColaProveedores = QData Proveedor -- Cola de Proveedores

-- Instancia con notación "continuation passing style"
instance FromJSON ItemStock where
  parseJSON = withObject "Item Stock" $ \i -> do
    codigoItem <- i .: "codigo_item"
    item <- i .: "item"
    marca <- i .: "marca"        
    rubro <- i .: "rubro"
    proveedor <- i .: "proveedor"
    uMed <- i .: "u_med"
    cantExistente <- i .: "cant_existente"
    vMin <- i .: "v_min"
    vMax <- i .: "v_max"
    precioU <- i .: "precio_u"
    pGanancia <- i .: "p_ganancia"
    return ItemStock{..}  

instance ToJSON ItemStock where 
  toJSON ItemStock{..} = object [    
    "codigo_item" .= codigoItem,
    "item" .= item,
    "marca" .= marca,
    "rubro" .= rubro,
    "proveedor" .= proveedor,
    "u_med" .= uMed,
    "cant_existente" .= cantExistente,
    "v_min" .= vMin,
    "v_max" .= vMax,
    "precio_u" .= precioU,
    "p_ganancia" .= pGanancia
    ]

-- Instancia con notación "Haskell continuation passing style" ?
instance FromJSON Proveedor where
  parseJSON = withObject "Proveedor" $ \o ->
      Proveedor <$> o .: "codigo_proveedor"
            <*> o .: "nombre"
            <*> o .: "direccion"
            <*> o .: "telefono"            

instance ToJSON Proveedor where
  toJSON Proveedor {..} = object [
    "codigo_proveedor" .= codigoProveedor,
    "nombre" .= nombre,
    "direccion" .= direccion,
    "telefono" .= telefono
    ]
--___________________________________________________________________________________________