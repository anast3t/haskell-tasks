myhead:: [a] -> a
myhead [] = error "No elements!"
myhead (x:_) = x

mytail:: [a] -> [a]
mytail [] = error "No elements!"
mytail (_:xs) = xs

myFoldr:: (a -> b -> a) -> a -> [b] -> a
myFoldr f accum [] = accum 
myFoldr f accum (x:xs) = myFoldr f accum xs `f` x -- @машинописный обратный апостроф@, код &#96; (шестнадцатеричный U+0060) Как его вообще предпологается втыкать?

myFoldl:: (a -> b -> a) -> a -> [b] -> a
myFoldl f accum [] = accum 
myFoldl f accum (x:xs) = myFoldl f (f accum x) xs

mylength:: [a] -> Integer
mylength [] = 0
mylength xs = myFoldl (\x y -> x + 1) 0 xs

myappend:: [a] -> a -> [a]
myappend [] n = [n]
myappend (x:[]) n = x:[n]
myappend (x:xs) n = x:myappend xs n

myconcat:: [a] -> [a] -> [a]
myconcat (x:[]) ys = x:ys
myconcat (x:xs) ys = x:myconcat xs ys

mynull:: [a] -> Bool
mynull [] = True
mynull _ = False

myelem::(Eq a) => [a] -> a -> Bool
myelem [] _ = False
myelem (x:xs) n = if x == n then True else myelem xs n

mylast:: [a] -> a
mylast [] = error "No elements!"
mylast [x] = x
mylast (x:xs) = mylast xs

myinit:: [a] -> [a]
myinit [] = error "No elements!"
myinit (x:[]) = []
myinit (x:xs) = x : myinit xs

myreverse:: [a] -> [a] 
myreverse [] = error "No elements!"
myreverse xs = myFoldl (\m k -> k:m) [] xs --не до конца понимаю почему это работает, тк аккумулятор фолдла не может же быть списком???

myMap:: (a->b) -> [a] -> [b]
myMap _ [] = []
myMap func (x:xs) = func x : myMap func xs

myzip::[a]->[b]->[(a,b)]
myzip [] [] = []
myzip (x:_) [] = error "Length of strings are not equal! Second string is shorter."
myzip [] (y:_) = error "Length of strings are not equal! First string is shorter."
myzip (x:xs) (y:ys) = (x,y) : myzip xs ys


tester::Int -> Bool --примерочный тестер для фильтра
tester x = if x > 10 then True else False 

myfilter:: (a -> Bool) -> [a] -> [a]
myfilter func [] = []
myfilter func (x:xs) = if func x == True then x: myfilter func xs else myfilter func xs


get :: [a] -> Integer -> a
get [] _= error "No elements!"
get (x:xs) 0 = x 
get (x:xs) n = get xs (n-1)

mydrop :: [a] -> Integer -> [a] 
mydrop [] _= error "No elements!"
mydrop xs 0 = xs 
mydrop (x:xs) 1 = xs
mydrop (x:xs) n = mydrop xs (n-1)

mytake :: [a] -> Integer -> [a]
mytake [] _= error "No elements!" 
mytake (x:xs) 1 = [x]
mytake (x:xs) n = mytake xs (n-1)

mysplitAt :: [a] -> Integer -> ([a], [a])
mysplitAt [] _= error "No elements!" 
mysplitAt xs n = (mytake xs n, mydrop xs n)
