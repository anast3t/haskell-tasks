myMap :: (a -> b) -> [a] -> [b]

myMap f s = foldr func [] s where 
    func a b = (\x xs -> f x : xs) a b  
--вписывыем применимую к элементам функцию и список. При помощи фолдл применяем
--функцию 