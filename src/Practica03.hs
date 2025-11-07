module Practica03 where

--Funciones auxiliares

-- Función que verifica si un elemento pertenece a una lista
-- Se usa en:
--   - check
pertenece :: Eq a => a -> [a] -> Bool
pertenece _ [] = False
pertenece a (x:xs) = if x == a then True else (pertenece a xs) 

-- Función que quita los elementos repetidos de una lista
-- Se usa en:
--   - variables
check :: [String] -> [String]
check [] = []
check (x:xs) = if (pertenece x xs) then check xs else x:(check xs)

-- Función para concatenar 2 listas
-- Se usa en:
--   - variables
append :: [a] -> [a] -> [a]
append [] xs = xs
append (x:xs) ys = x : append xs ys

--Función para determinar los modelos (revisa estado por estado y si se cumple la propocsición lo agrega a los modelos)
-- Se usa en:
--   - modelos 
sirve :: Prop -> [Estado] -> [Estado]
sirve _ [] = []
sirve a (x:xs) = if(interpretacion a x) then x:(sirve a (xs)) else sirve a (xs)

--Funcion para determinar si dos listas de estados son iguales
-- Se usa en:
--   - tautologia
--   - consecuenciaLogica
iguales :: [Estado] -> [Estado] -> Bool
iguales [] [] = True
iguales (x:xs) [] = False
iguales [] (x:xs) = False
iguales (x:xs) (y:ys) = if (x==y) then (iguales xs ys) else False

--Funcion para pasar una listado de estados a una conjuncion
-- Se usa en:
--   - consecuenciaLogica
conj :: [Prop] -> Prop 
conj [x] = x
conj (x:xs) = (And x (conj xs))


-- Tipo de dato Prop
data Prop = 
    Var String |
    Cons Bool |
    Not Prop |
    And Prop Prop |
    Or Prop Prop |
    Impl Prop Prop |
    Syss Prop Prop 
    deriving (Eq)

-- Imprimir el tipo de dato Prop
instance Show Prop where
    show (Cons True) = "Verdadero"
    show (Cons False) = "Falso"
    show (Var p) = p
    show (Not p) = "¬" ++ show p 
    show (Or p q) = "(" ++ show p ++ " ∨ " ++ show q ++ ")"
    show (And p q) = "(" ++ show p ++ " ∧ " ++ show q ++ ")"
    show (Impl p q) = "(" ++ show p ++ " → " ++ show q ++ ")"
    show (Syss p q) = "(" ++ show p ++ " ↔ " ++ show q ++ ")"

-- Fórmulas proposicionales (Variables atómicas)
p, q, r, s, t, u :: Prop
p = Var "p"
q = Var "q"
r = Var "r"
s = Var "s"
t = Var "t"
u = Var "u"

-- Sinonimo para los estados
type Estado = [String]

-- Ejercicio 1
variables :: Prop -> [String]
variables (Cons True) = []
variables (Cons False) = []
variables (Var x) = [x]
variables (Not p) = check(variables p)
variables (Or p q) = check(append (variables p) (variables q))
variables (And p q) = check(append (variables p) (variables q))
variables (Impl p q) = check(append (variables p) (variables q))
variables (Syss p q) = check (append (variables p) (variables q))


-- Ejercicio 2
interpretacion :: Prop -> Estado -> Bool
interpretacion (Cons True) xs = True
interpretacion (Cons False) xs = False
interpretacion (Var x) xs = pertenece x xs
interpretacion (Not p) xs = not (interpretacion p xs)
interpretacion (Or p q) xs = (interpretacion p xs) || (interpretacion q xs)
interpretacion (And p q) xs = (interpretacion p xs) && (interpretacion q xs)
interpretacion (Impl p q) xs = (not(interpretacion p xs)) || (interpretacion q xs)
interpretacion (Syss p q) xs = (interpretacion (Impl p q) xs) && (interpretacion (Impl q p) xs) 



-- Ejercicio 3
estadosPosibles :: Prop -> [Estado]
estadosPosibles (Cons True) = conjuntoPotencia ([])
estadosPosibles (Cons False) = conjuntoPotencia ([])
estadosPosibles (Var x) = conjuntoPotencia ([x])
estadosPosibles a = conjuntoPotencia (variables a) 



-- Ejercicio 4
modelos :: Prop -> [Estado]
modelos (Cons True) = []
modelos (Cons False) = []
modelos (Var x) = [[x]]
modelos a = sirve a (estadosPosibles a) 

-- Ejercicio 5
sonEquivalentes :: Prop -> Prop -> Bool
sonEquivalentes x y = tautologia (Syss x y)

-- Ejercicio 6
tautologia :: Prop -> Bool
tautologia (Cons True) = True
tautologia (Cons False) = False
tautologia (Var x) = False
tautologia a = iguales (modelos a) (estadosPosibles a)

-- Ejercicio 7
consecuenciaLogica :: [Prop] -> Prop -> Bool
consecuenciaLogica [] a = False 
consecuenciaLogica (x:xs) a = tautologia((Impl (conj(x:xs)) a))

--Funcion auxiliar
conjuntoPotencia :: [a] -> [[a]]
conjuntoPotencia [] = [[]]
conjuntoPotencia (x:xs) = [(x:ys) | ys <- conjuntoPotencia xs] ++ conjuntoPotencia xs

