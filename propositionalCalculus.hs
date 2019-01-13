module Prop
(varValue 
,evalWithEnv
,allVars
,bind 
,allBools
,allEnvs
,isTautology
,isSatisfiable
,isContradiction
,semanticallyImplies 
,semanticallyEquivalent
,isAxiom
,modusPonens
,proofFrom) where

type Name = String

data Prop = Const Bool
          | Var Name
          | Not Prop
          | And Prop Prop
          | Or Prop Prop
          | Implies Prop Prop

infixr 7 `Implies`
infixl 9 `And`
infixl 8 `Or`

instance Show Prop where
    show (Const b)          = if b then "T" else "F"
    show (Var x)            = x
    show (Not fi)           = '¬' : show fi
    show (fi `And` psi)     = show fi ++ " & " ++ show psi
    show (fi `Or` psi)      = "(" ++ show fi ++ " ∨ " ++ show psi ++ ")"
    show (fi `Implies` psi) = "(" ++ show fi ++ " → " ++ show psi ++ ")"

instance Eq Prop where
    (Const x)       == (Const y)       = x == y
    (Var x)         == (Var y)         = x == y
    (Not x1)        == (Not x2)        = x1 == x2
    (And x1 y1)     == (And x2 y2)     = x1 == x2 && y1 == y2
    (Or x1 y1)      == (Or x2 y2)      = x1 == x2 && y1 == y2
    (Implies x1 y1) == (Implies x2 y2) = x1 == x2 && y1 == y2
    _               == _               = False

type Environment = [(Name, Bool)]

varValue :: Environment -> Name -> Bool
varValue [] _ = False
varValue (x:xs) n = if (fst x == n) then snd x else varValue xs n

evalWithEnv :: Environment -> Prop -> Bool
evalWithEnv _ (Const x)     = x
evalWithEnv e (Var x)       = varValue e x
evalWithEnv e (Not x)       = not (evalWithEnv e x)
evalWithEnv e (And x y)     = evalWithEnv e x && evalWithEnv e y
evalWithEnv e (Or x y)      = evalWithEnv e x || evalWithEnv e y
evalWithEnv e (Implies x y) = not (evalWithEnv e x == True && evalWithEnv e y == False)

allVars :: Prop -> [String]
allVars (Const _)       = []
allVars (Var n)         = [n]
allVars (Not p)         = allVars p
allVars (And p1 p2)     = allVars p1 ++ allVars p2
allVars (Or p1 p2)      = allVars p1 ++ allVars p2
allVars (Implies p1 p2) = allVars p1 ++ allVars p2

bind :: [Name] -> [Bool] -> Environment
bind []    _      = []
bind _     []     = []
bind names values = zipWith (,) names values

allBools :: Int -> [[Bool]]
allBools 0 = []
allBools 1 = [[True], [False]]
allBools n = (map (\x -> True : x) nxt) ++ (map (\x -> False : x) nxt)
    where nxt = allBools (n-1)

allEnvs :: [Name] -> [Environment]
allEnvs []    = []
allEnvs names = map (zipWith (,) names) (allBools (length names))

value :: Prop -> [Bool]
value prop = map (\x -> evalWithEnv x prop) (allEnvs (allVars prop))

isTautology :: Prop -> Bool
isTautology (Const x) = x
isTautology x         = foldl1 (&&) (value x)

isSatisfiable :: Prop -> Bool
isSatisfiable (Const x) = x
isSatisfiable x         = foldl1 (||) (value x)

isContradiction :: Prop -> Bool
isContradiction (Const x) = not x
isContradiction x         = foldl1 (||) (value x) == False


semanticallyImplies :: Prop -> Prop -> Bool
semanticallyImplies (Const x) (Const y) = x && y
semanticallyImplies (Const x) y         = x && foldl1 (&&) (value y)
semanticallyImplies x         (Const y) = foldl1 (&&) (value x) && y
semanticallyImplies x         y         = foldl1 (&&) (value (Implies x y))

semanticallyEquivalent :: Prop -> Prop -> Bool
semanticallyEquivalent x y = semanticallyImplies x y && semanticallyImplies y x

isAxiom :: Prop -> Bool
isAxiom (Implies (Implies a b) (Implies (Implies c (Not d)) (Not e))) = a == c && c == e && b == d
isAxiom (Implies (Implies a b) (Implies (Implies c d) (Implies (Or e f) g))) = a == e && b == d && d == g && c == f
isAxiom (Implies a (Implies (Not b) _)) =  a == b
isAxiom (Implies (Implies x (Implies y z)) (Implies (Implies a b) (Implies c d))) = x == a && a == c && y == b && z == d
isAxiom (Implies x (Implies y (And w z))) = x == w && y == z
isAxiom (Implies (And x y) z) = x == z || y == z
isAxiom (Implies z (Or x y))    = z == x || z == y
isAxiom (Or a (Not b)) = a == b
isAxiom (Implies x (Implies _ z)) = x == z
isAxiom _ = False

modusPonens :: Prop -> Prop -> Prop -> Bool
modusPonens x (Implies y z) w = x == y && z == w
modusPonens _ _             _ = False

proofFrom :: [Prop] -> [Prop] -> Bool
proofFrom _ [] = True
proofFrom r (x:xs) = if (isAxiom x || x `elem` r || check r) then proofFrom (x:r) xs else False
    where check :: [Prop] -> Bool
          check [] = False
          check (g:gs) = (any (\e -> (e == (Implies g x)) || (g == (Implies e x)) ) gs) || check gs