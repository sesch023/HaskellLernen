-- 1
data Punkt = Punkt Float Float
data ZweiDObjekt = Kreis Punkt Float | Rechteck Punkt Punkt | Dreieck Punkt Punkt Punkt

flaeche :: ZweiDObjekt -> Float
flaeche (Kreis _ r) = pi*r^2
flaeche (Rechteck (Punkt a b) (Punkt c d)) = (abs(c - a)) * (abs(d - b))
flaeche (Dreieck (Punkt x1 y1) (Punkt x2 y2) (Punkt x3 y3)) = abs(1.0/2.0*(x1*(x2-y3)+x2*(y3-y1)+x3*(y1-y2)))

-- 2
mymult :: Int -> Float -> Float
mymult a b = (fromIntegral a) * b

-- 3
data Polynom
    = Const Float
    | PowerOfX Float Int
    | Sum Polynom Polynom
    deriving (Eq)

instance Show Polynom where
    show (Const x) = "(" ++ show x ++ ")"
    show (PowerOfX x y) = "(" ++ show x ++ "^" ++ show y ++ ")"
    show (Sum x y) = "(" ++ show x ++ " + " ++ show y ++ ")"

test_poly = Sum (Sum (Const 42) (PowerOfX 11.11 1)) (PowerOfX 22.22 2)

poly2list :: Polynom -> [Polynom]
poly2list (Sum a b) = (poly2list a) ++ (poly2list b)
poly2list x = [x]

list2poly :: [Polynom] -> Polynom
list2poly [] = Const 0.0
list2poly x = foldr (\x acc -> Sum x acc) (last x) (init x)

ableiten :: Polynom -> Polynom
ableiten x = ableitenRec $ cleanPoly$ list2poly $ poly2list x

ableitenRec :: Polynom -> Polynom
ableitenRec (Const _) = Const 0.0
ableitenRec (PowerOfX a 1) = Const a
ableitenRec (PowerOfX a b) = PowerOfX (a*(fromIntegral b)) (b-1)
ableitenRec (Sum (Const _) b) = (ableitenRec b)
ableitenRec (Sum a (Const _)) = (ableitenRec a)
ableitenRec (Sum a b) = Sum (ableitenRec a) (ableitenRec b)

test_unclean = Sum (Sum (Sum (Const 5.0) (Const 6.0)) (Sum (Const 1.0) (Const 2.0))) (Sum (PowerOfX 5 4) (PowerOfX 10 4))

cleanPoly :: Polynom -> Polynom
cleanPoly x = aux x (Const 0.0)
    where 
        aux x y
            | x == y = x
            | otherwise = aux (cleanPolySingle x) x

cleanPolySingle :: Polynom -> Polynom
cleanPolySingle (Sum (Const x) (Const y)) = Const (x + y)
cleanPolySingle (Sum (PowerOfX x a) (PowerOfX y b))
    | a == b = PowerOfX (x + y) a
    | otherwise = Sum (PowerOfX x a) (PowerOfX y b)
cleanPolySingle (Sum a b) = Sum (cleanPolySingle a) (cleanPolySingle b)
