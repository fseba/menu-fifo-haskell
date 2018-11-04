------------------------------------------------------------------------------
-- Modulo      :  Model.Categories
-- Programador :  Sebastián Uriel Flores
-- Estabilidad :  experimental
-- Portabilidad:  experimental
--
-- Programacion Avanzada en Haskell - Ing. Informática. 2018
-- 
-----------------------------------------------------------------------------    
module Model.Categories where

-- 1) Natural isomorphisms --------------------------------------------------

swap :: (vv,mm) -> (mm,vv)
swap (a,b) = (b,a)

shiftr3 :: (t1, t2, t) -> (t, t1, t2)
shiftr3 (a,b,c) = (c,a,b)

assoc :: ((a, t), t1) -> (a, (t, t1))
assoc = split ( fst . fst ) (split ( snd . fst ) snd )

assocr :: (t, t1, t2) -> (t, (t1, t2))
assocr (a,b,c) = (a,(b,c))

unassocr :: (t, (t1, t2)) -> (t, t1, t2)
unassocr (a,(b,c)) = (a,b,c)

assocl :: (t1, t2, t) -> ((t1, t2), t)
assocl (a,b,c) = ((a,b),c)

unassocl :: ((t, t1), t2) -> (t, t1, t2)
unassocl ((a,b),c) = (a,b,c)

assocr4 (a,b,c,d) = (a,(b,c,d))
unassocr4 (a,(b,c,d)) = (a,b,c,d)

assocr6 (a,b,c,d,e,f) = (a,(b,c,d,e,f))
unassocr6 (a,(b,c,d,e,f)) = (a,b,c,d,e,f)

-- 2) Useful fuctions --------------------------------------------------

-- ?? cond es una funcion que toma cuatro argumentos: un predicado booleano unario, una funcion f, una funcion g, y un valor x aplicable con f y g. Si se cumple p x entonces retorna f x, en caso contrario retorna g x
cond :: (b -> Bool) -> (b -> c) -> (b -> c) -> b -> c
cond p f g = ((either f g) . (grd p))
--Ej: cond (<2) (+2) (+3) 1 -> 3

-- ?? split es una funcion que toma tres argumentos: una funcion f, una funcion g, y un valor x. Devuelve una tupla donde el primer argumento es el resultado de aplicar f al valor x y el segundo argumento es el resultado de aplicar g al valor x.
split :: (t2 -> t) -> (t2 -> t1) -> t2 -> (t, t1)
split f g x = (f x, g x)
--Ej: split (+2) (*2) 5 ->(7,10)

-- ?? ev es una funcion que toma una tupla, donde el primer elemento es una funcion f unaria, y el segundo elemento es un valor x aplicable con f. Devuelve el resultado de aplicar f x. El operador ($) permite su asociatividad a derecha.
ev :: (a -> b,a) -> b
ev = uncurry ($)
-- Ej: ($) (2+) 6 -> 8
-- Ej: uncurry (+) (1,4) -> 5
-- Ej: ev ((6/),3) -> 2.0
-- Ej: ev ((/6),3) -> 0.5

-- ?? expn es una funcion de tres parametros: una funcion f unaria, una funcion g unaria, y un valor x. Retorna la composicion de funciones f(g x), es decir f aplicado al resultado de g x.
expn :: (b -> c) -> (a -> b) -> a -> c
expn f = (\g -> f . g)
-- Ej: expn (+2) (*3) 5 -> 17

-- ?? grd toma dos argumentos: un predicado booleano unario, y un valor x. Si se cumple p x entonces retorna un valor de tipo Left x, y sino devuelve un valor de tipo Right x
grd :: (b -> Bool) -> b -> Either b b
grd p x = if p x then Left x else Right x
-- Ej: grd (>1) 3 -> Left 3
-- Ej: grd (<1) 3 -> Right 3