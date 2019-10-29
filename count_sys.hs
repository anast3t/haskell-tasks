import Data.Char

conv:: [Char] -> [Int]
conv str = map func str where
    func x  | ((ox >= 48) && (ox <= 57)) = (ox - 48)
            | ((ox >= 97) && (ox <= 122)) = (ox - 87)
            | ((ox >= 65) && (ox <= 90)) = (ox - 29) 
            | otherwise = error "String not correct" where 
                ox = ord x
--
conv1:: Char -> Int
conv1 m = func m where
    func x  | ((ox >= 48) && (ox <= 57)) = (ox - 48)
            | ((ox >= 97) && (ox <= 122)) = (ox - 87)
            | ((ox >= 65) && (ox <= 90)) = (ox - 29) 
            | otherwise = error "Digit not correct" where 
                ox = ord x
--
dECconvint:: [Int] -> Int
dECconvint [x] = x
dECconvint (x:xs) = x*(10^(length xs)) + dECconvint xs
--
bconv:: [Int] -> [Char]
bconv str = map func str where
    func x  | ((x >= 0) && (x <= 9)) = chr (x + 48)
            | ((x >= 10) && (x <= 35)) = chr (x + 87)
            | ((x >= 36) && (x <= 61)) = chr (x + 29) 
            | otherwise = error "String not correct"
--
chkbase:: Int -> Bool
chkbase base| ((base >= 1) && (base <= 62)) = True
            | otherwise = error "Base not correct" 
--
checker:: Int -> [Char] -> [Char]
checker base str = (filter (\x -> (if ((conv1 x) >= base) then error "String numbers are bigger then base (Incorrect string input)" else False)) str)
--
checker1:: [Char] -> [Char]
checker1 str = (filter (\x -> (if ((conv1 x) > 1) then error "String numbers are bigger then base (Incorrect string input)" else False)) str)
---------------------------------------------------------------
toDecimal:: Int -> [Char] -> [Char]
toDecimal 1 str = if (checker1 str == []) then show(todec2 1 str) else error "0_0" --don't beat me pls :(
toDecimal base str = if chkbase base then (if (checker base str == []) then show (todec2 base str) else error "0_0") else error "0_0" 

todec2:: Int -> [Char] -> Int
todec2 1 [x] = ((conv1 x) - 1)
todec2 base [x] = (conv1 x)
todec2 base (x:xs) = (conv1 x)*(base^(length xs)) + todec2 base xs
---------------------------------------------------------------
fromDecimal:: Int -> [Char] -> [Char]
fromDecimal base xs = if chkbase base then (if (checker 10 xs == []) then (bconv(frd base (dECconvint(conv xs)))) else error "0_0") else error "0_0"

frd::Int -> Int -> [Int]
frd 1 0 = [1]
frd 1 x = 1:frd 1 (x-1)
frd base x = if x >= base then (frd base (x `div` base) ++ [x `mod` base]) else [x]
---------------------------------------------------------------
convertFromTo:: Int -> Int -> [Char] -> [Char]
convertFromTo baseF baseT str = fromDecimal baseT (toDecimal baseF str)
---------------------------------------------------------------
--все вылезающие функции потом причешу (время три утра я в щи) :( 
