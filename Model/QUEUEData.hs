------------------------------------------------------------------------------
-- | 
-- Module      :  QUEUEData
-- Maintainer  :  
-- Stability   :  experimental
-- Portability :  experimental
--
--Optativa Haskell en Cs. de la Comp . Lab 2014
-- This module implements finite Queue (LIFO structure)  
-----------------------------------------------------------------------------         
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Model.QUEUEData where

import Model.Categories
import Data.List
import Data.Aeson

-- 0) Dinamic queue - Data definition

data QData a =  EmptyQ | Q [a]  deriving  Show --(Eq,Ord,Show)
data QData2 a = Q2[a] deriving Show

-- 1) Constructors ----------------------------------------------------------
--emptyQ :: QData a
--emptyQ = EmptyQ

ltoQ :: [a]-> QData a
ltoQ l    = Q l

qtoL :: QData a -> [a]
qtoL (Q l) = l

-- 2) Programmer definitions of (==) (<=) 

instance Eq a => Eq (QData a)
        where s == r = s <= r && r <= s

instance Eq a => Ord (QData a)        
        where   EmptyQ <= EmptyQ = True  
                EmptyQ <= (Q _) = True  
                (Q _) <= EmptyQ = False
                (Q s) <= (Q r) = any (== s) (tails r)


--3) Alternative Data Queue Definition (recursive) ----------------------------------------------------------
-- NO SE UTILIZA
--data QDataR a = 

-- 4) Standard Queue' Functions ----------------------------------------------------

showQ :: (Show a) => String -> QData a -> String
showQ title q = "*   " ++ title ++ ": " ++ case q of
      EmptyQ -> "Cola Vacia"
      Q l -> foldr (\e ac -> if ac == "" then e else e ++ " -> " ++ ac) "" lEnumStr where
        lEnum = zip [1..] l
        lEnumStr = map (\(n,e) -> show n ++ "." ++ show e) lEnum
      --Q l -> foldr (\e ac -> if ac == "" then (show e) else (show e) ++ " -> " ++ ac) "" l

-- Funciones de insercion
insertQ :: Eq a => a -> QData a -> QData a
insertQ a EmptyQ = Q [a]
insertQ a (Q s) = Q (s ++ [a])

insertQR :: Eq a => a -> QData a -> QData a
insertQR e (EmptyQ) = Q [e]
insertQR e (Q (x:xs)) = fAux1 x (insertQR e (Q xs))

fAux1 :: a -> QData a -> QData a
fAux1 e EmptyQ = Q [e]
fAux1 e (Q l) = Q ([e] ++ l)

-- Funciones de supresion
supresQ :: QData a -> QData a
supresQ EmptyQ = EmptyQ
supresQ (Q [x]) = EmptyQ
supresQ (Q (_:xs)) = Q xs

supresQR :: QData a -> QData a
supresQR EmptyQ = EmptyQ
supresQR (Q [x]) = EmptyQ
supresQR (Q l) = fAux2 (last l) (supresQR (Q (init l)))

fAux2 :: a -> QData a -> QData a
fAux2 e EmptyQ = Q [e]
fAux2 e (Q l) = Q (l++[e])

-- Funciones de observacion
copyQ :: QData a -> a
copyQ (Q s) = (head s)

copyQR :: QData a -> a
copyQR (Q [x]) = x
copyQR (Q l) = copyQR (Q (init l))

-- Predicado para saber si esta vacia
isEmptyQ :: QData a -> Bool
isEmptyQ EmptyQ = True
isEmptyQ _ = False

-- La funcion isFull(Q s) no es necesaria

-- 5) Useful non Standard Queue' Functions ------------------------------------------------------

-- | Membership (pertenencia)
-- inQ 2 (Q [1,2,3]) retorna True
inQ :: (Eq a) => a -> QData a -> Bool
inQ a (Q s) = elem a s

inQR :: (Eq a) => a -> QData a -> Bool
inQR a (EmptyQ) = False
inQR a (Q (x:xs))| (x==a) = True
                 | otherwise = inQ a (Q xs)

notinQ :: Eq a => a -> QData a -> Bool
notinQ a s = not (inQ a s)

notinQR :: Eq a => a -> QData a -> Bool
notinQR a (EmptyQ) = True 
notinQR a (Q (x:xs)) = notinQ a (Q xs) && x /= a

--  | cardinality (cantidad de elementos en la estructura)
cardQ :: QData a -> Int
cardQ EmptyQ = 0
cardQ (Q s) = length s

cardQ' :: QData a -> Int
cardQ' (Q s) = sum (map (const 1) s)

cardQR :: QData a -> Int
cardQR (EmptyQ) = 0
cardQR (Q (x:xs)) = 1 + cardQR (Q xs)

-- | strenght (Int: use comprehension list)
--   the function that pairs all elements of a structure with one particular element

-- left       (Ej: lstrQ (1, Q[4,3,2])) -> Q [(1,4),(1,3),(1,2)]
lstrQ :: (a, QData b) -> QData (a,b)
lstrQ (a, Q b) = (Q [(a,y) | y <- b])

lstrQR :: (a, QData b) -> QData (a,b)
lstrQR (_, (EmptyQ)) = (EmptyQ)
lstrQR (a, (Q (x:xs))) = fAux1 (a, x) (lstrQR (a, (Q xs)))

-- right      (Ej: rstrQ (Q[4,3,2], 5)) -> Q [(4,5),(3,5),(2,5)]
rstrQ :: (QData b, a) -> QData (b,a)
rstrQ (Q b, a) = (Q [(x,a) | x <- b])

rstrQR :: (QData b, a) -> QData (b,a)
rstrQR ((EmptyQ),_) = (EmptyQ)
rstrQR ((Q(x:xs)),a) = fAux1 (x,a) (rstrQ ((Q xs),a))

--6) Hight Level functions -------------------------------------------------
-- | Mapping
mapQ :: (a->b)->QData a -> QData b
mapQ _ EmptyQ = EmptyQ
mapQ f (Q s) = Q(map f s)

mapQR :: (a->b)->QData a -> QData b
mapQR f EmptyQ = EmptyQ
mapQR f (Q (x:xs)) = fAux1 (f x) (mapQR f (Q xs) )

-- | Folding
foldQ ::(a -> b -> b) -> b -> QData a -> b   
foldQ f e EmptyQ = e
foldQ f e (Q lista) = foldr f e lista

foldQR ::(a -> b -> b) -> b -> QData a -> b   
foldQR f e EmptyQ = e
foldQR f e (Q (x:xs)) = f x (foldQR f e (Q xs))

mapQ' :: (a -> b)->QData a->QData b
mapQ' f s = foldQ (\ x xs -> ((fAux1. f) x) xs) (EmptyQ) s

-- (Int: Use foldQ )

--  | filter
filterQ :: Eq a => (a -> Bool) -> QData a -> QData a
filterQ  p s = foldQ (\x xs -> if p x then (fAux1 x xs) else xs) (EmptyQ) s

filterQR :: Eq a => (a -> Bool) -> QData a -> QData a
filterQR p (EmptyQ) = (EmptyQ)
filterQR p (Q(x:xs)) = if p x then (fAux1 x (filterQR p (Q xs))) else filterQR p (Q xs)

-- (Int: Use foldQ )

allQ :: (a -> Bool) -> QData a -> Bool
allQ p (Q s) = all p s

allQR :: (a -> Bool) -> QData a -> Bool
allQR p (Q[x]) = p x
allQR p (Q (x:xs)) = allQR p (Q xs) && (p x)

-- 7) More Useful functions ------------------------------------------------------
-- | flatten  (Transforma una cola en una lista)
flattenQ :: QData a -> [a] 
flattenQ (Q s) = s  

-- | flatr  (Transforma los pares con asociatividad derecha de una cola en tri-tuplas. Elimina asociatividad der.)
-- Usar mapQ para Colas
flatr:: (Eq a, Eq b, Eq c) => QData (a,(b,c))-> QData (a,b,c)
flatr s = mapQ unassocr s

-- (Int: Use unassocr, Categories module)

flatl:: (Eq a, Eq b, Eq c) => QData ((a,b),c)-> QData (a,b,c)
flatl s = mapQ unassocl s

-- (Int: Use unassocl, Categories module)

-- | Zips (Toma dos colas y retorna una cola de pares - actúa como un cierre o cremallera)
zipS :: QData a -> QData b -> QData (a,b)
zipS (Q a) (Q b)= Q(zip a b)

unzipS:: (Eq b, Eq a) => QData (a,b) -> (QData a,QData b)
unzipS a = split (mapQ(fst)) (mapQ(snd)) a 

--(Int: puede usar split function - Module Categories )

-- | zipWithallQ (Toma dos colas y genera una cola donde cada elemento es un par formado por 
-- un elemento de la primera cola y Toda la 2da cola)
-- Ej: zipWithallQ (Q  [1,2]) (Q [3,4,5]) -> Q [(1, Q [3,4,5]), (2, Q [3,4,5] )]

zipWithallQ :: QData a -> QData b -> QData (a,QData b)
zipWithallQ (EmptyQ) _ = (EmptyQ)
zipWithallQ (Q (x:xs)) b = fAux1 (x,b) (zipWithallQ (Q xs) b) 

-- 8) Notation Shortcuts------------------------------------------------
a -| s = inQ a s
a -|| s = notinQ a s

{--
instance FromJSON Item_Stock where
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
    return Item_Stock{..}  

instance ToJSON Item_Stock where 
  toJSON Item_Stock{..} = object [    
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
--}
