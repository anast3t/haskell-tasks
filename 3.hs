myMap :: (a -> b) -> [a] -> [b]

myMap f s = foldr func [] s where 
    func a b = (\x xs -> f x : xs) a b
