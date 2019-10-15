myhead:: [a] -> a
myhead [] = error "Нет элементов!"
myhead (x:xs) = x

mytail:: [a] -> [a]
mytail [] = error "Нет элементов!"
mytail (x:xs) = xs

mylength:: [a] -> Int
mylength [] = error "Нет элементов!"
mylength (x:xs) = 1 + mylength xs

myappend:: [a] -> a -> [a]
myappend [] _ = error "Нет элементов!"
myappend xs x = xs ++ [x]

myconcat:: [a] -> [a] -> [a]
myconcat [] [] = error "Нет элементов!"
myconcat xs ys = xs++ys

mynull:: [a] -> Bool
mynull [] = True
mynull _ = False

myelem::(Eq a) => [a] -> a -> Bool
myelem [] _ = False
myelem (x:xs) n = if x == n then True else myelem xs n

mylast:: [a] -> a
mylast [] = error "Нет элементов!"
mylast (x:[]) = x
mylast (x:xs) = mylast xs

myinit:: [a] -> [a]
myinit [] = error "Нет элементов!"
myinit (x:[]) = []
myinit (x:xs) = x : myinit xs

myreverse:: [a] -> [a]
myreverse [] = error "Нет элементов!"
myreverse (x:[]) = [x]
myreverse (x:xs) = myreverse xs ++ [x]

myMap:: (a->b) -> [a] -> [b]
myMap _ [] = error "Нет элементов!"
myMap func (x:[]) = [func x]
myMap func (x:xs) = func x : myMap func xs

myzip::[Int]->[Int]->[(Int,Int)]
myzip [] [] = error "Нет элементов!"
myzip (x:_) [] = error "Нет элементов во втором списке!"
myzip [] (y:_) = error "Нет элементов в первом списке!"
myzip (x:[]) (y:[]) = [(x,y)]
myzip (x:xs) (y:[]) = [(x,y)]
myzip (x:[]) (y:ys) = [(x,y)]
myzip (x:xs) (y:ys) = (x,y) : myzip xs ys

tester::Int -> Bool --для фильтра
tester x = if x > 10 then True else False 

myfilter:: (Int -> Bool) -> [Int] -> [Int]
myfilter func [] = error "Нет элементов!" --виртуоз по костылям
myfilter func (x:[]) = case func x of
                        True -> [x]
                        False -> []
myfilter func (x:xs) = case func x of
                        True -> x: myfilter func xs
                        False -> myfilter func xs

--

get :: [Int] -> Int -> Int
get [] _= error "Нет элементов!"
get (x:xs) n = case n of
                  0 -> x
                  otherwise -> get xs (n-1)

--

mydrop :: [Int] -> Int -> [Int]
mydrop [] _= error "Нет элементов!"
mydrop xs 0 = xs 
mydrop (x:xs) n = case n of
                  1 -> xs
                  otherwise -> mydrop xs (n-1)

--

mytake :: [Int] -> Int -> [Int]
mytake [] _= error "Нет элементов!" 
mytake (x:xs) n = case n of
                  1 -> [x]
                  otherwise -> x : mytake xs (n-1)

--

mysplitAt :: [Int] -> Int -> ([Int], [Int])
mysplitAt [] _= error "Нет элементов!" 
mysplitAt xs n = (mytake xs n, mydrop xs n)
