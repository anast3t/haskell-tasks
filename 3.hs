myMapR :: (a -> b) -> [a] -> [b]
myMapR f xs = myFoldr (\s x -> f x:s) [] xs

myMapL :: (a -> b) -> [a] -> [b]
myMapL f xs = myreverse (myFoldl (\s x -> f x:s) [] xs)
-------------------------------------------
myFoldr:: (a -> b -> a) -> a -> [b] -> a
myFoldr f accum [] = accum 
myFoldr f accum (x:xs) = myFoldr f accum xs `f` x

myFoldl:: (a -> b -> a) -> a -> [b] -> a
myFoldl f accum [] = accum 
myFoldl f accum (x:xs) = myFoldl f (f accum x) xs

myreverse:: [a] -> [a] 
myreverse [] = error "No elements!"
myreverse xs = myFoldl (\m k -> k:m) [] xs