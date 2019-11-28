data Complex a = ToComplex{
    real::a,
    imaginary::a
}
instance (Show a, Eq a, Ord a, Num a, Fractional a) => Show(Complex a) where 
    show (ToComplex real 0) = show(real)
    show (ToComplex 0 1) = " i"
    show (ToComplex 0 (-1)) = " - i"
    show (ToComplex 0 im) = show im ++ " i"
    show (ToComplex real im) | im == 1 = show real ++ " + i"
                             | im == (-1) = show real ++ " - i"
                             | im < 0 = show real ++ show im ++ "i"
                             | im > 0 = show real ++ " + " ++ show im ++ "i"

--
instance (Show a, Eq a, Ord a, Num a, Fractional a) => Ord(Complex a) where 
    (<)  (ToComplex r1 i1) (ToComplex r2 i2) = ((r1*r1 + i1*i1) <  (r2*r2 + i2*i2))
    (>)  (ToComplex r1 i1) (ToComplex r2 i2) = ((r1*r1 + i1*i1) >  (r2*r2 + i2*i2))
    (<=) (ToComplex r1 i1) (ToComplex r2 i2) = ((r1*r1 + i1*i1) <= (r2*r2 + i2*i2))
    (>=) (ToComplex r1 i1) (ToComplex r2 i2) = ((r1*r1 + i1*i1) >= (r2*r2 + i2*i2))
--
instance (Show a, Eq a, Ord a, Num a, Fractional a)=> Eq(Complex a) where
    (==) (ToComplex r1 i1) (ToComplex r2 i2) = if r1==r2 && i1==i2 then True else False
--
instance (Show a, Eq a, Ord a, Num a, Fractional a)=> Num(Complex a) where
    (+) (ToComplex r1 i1) (ToComplex r2 i2) = ToComplex (r1+r2) (i1+i2)
    (-) (ToComplex r1 i1) (ToComplex r2 i2) = ToComplex (r1-r2) (i1-i2)
    (*) (ToComplex r1 i1) (ToComplex r2 i2) = ToComplex (r1*r2 - i1*i2)  (r1*i2 + r2*i1)
    (abs) _ = undefined
    (signum) _ = undefined
    (fromInteger) _ = undefined
--
instance (Show a, Eq a, Ord a, Num a, Fractional a) => Fractional(Complex a) where
    (/) (ToComplex r1 i1) (ToComplex r2 i2) = ToComplex ((r1*r2 + i1*i2)/(r2*r2 + i2*i2)) ((i1*r2 - r1*i2)/(r2*r2 + i2*i2))
    (fromRational) _ = undefined
--



data QuantumState a = ToQS{
    vector::a,
    name::String
}
instance Functor QuantumState  where
    fmap f (ToQS c n) =ToQS (f c) n 
--
instance (Show a) => Show (QuantumState a) where
    show (ToQS c n) = show n ++ ": " ++ show c
--



data Qubit a = ToQB{
    states::[QuantumState a]
}
instance (Num a) => Num (Qubit a) where
    (+) (ToQB s1) (ToQB s2) = ToQB (s1++s2)
--
instance (Show a) => Show (Qubit a) where
    show (ToQB s) = show s 
--



toList::Qubit (Complex a) -> [Complex a]
toList (ToQB []) = []
toList (ToQB (x:xs)) = (vector x) : toList (ToQB xs)

toLabelList::Qubit(Complex a) -> [String]
toLabelList (ToQB []) = []
toLabelList (ToQB (x:xs)) = (name x) : toLabelList (ToQB xs)

fromList::(Show a, Num a, Ord a, Fractional a) => [Complex a]->[String]->Qubit (Complex a) -- он меня заставил все это пробить, почему?
fromList (x:[]) (s:[]) = ToQB ([ToQS x s]) 
fromList (x:xs) (s:ss) = ToQB ([ToQS x s]) + (fromList xs ss)  

toPairList:: Qubit (Complex a)->[(Complex a,String)]
toPairList (ToQB []) = []
toPairList (ToQB (x:xs)) = (vector x, name x) : toPairList (ToQB xs)

fromPairList::(Show a, Num a, Ord a, Fractional a) => [(Complex a,String)] -> Qubit (Complex a)
fromPairList ((x,s):[]) = ToQB ([ToQS x s]) 
fromPairList ((x,s):as) = ToQB ([ToQS x s]) + (fromPairList as) 

scalarProduct:: Qubit (Complex a) -> Qubit (Complex a) -> a
scalarProduct [] [] = 0
scalarProduct ((ToQS (ToComplex r1 i1) _):xs) ((ToQS (ToComplex r2 i2) _):ys) = r1*r2 + i1*i2 + scalarProduct ToQS(xs) ToQS(ys) 

--examples and bases
x = ToComplex 1 1
y = ToComplex 1.0 2.0
z = ToComplex 2.0 3.0
a = ToQS y "String"
b = ToQS z "str1"
c = ToQS (y*z) "bred"
d = ToQS (y+z) "asdasd"
znch = [x,y,z]
str = ["str1", "foo", "asd"]
abc = ToQB [a,b,c,d]
pair = toPairList abc



