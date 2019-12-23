import Data.Char
--READY
conv:: [Char] -> [Int] --преобразование всей строки
conv str = map func str where
    func x  | ((ox >= ord '0') && (ox <= ord '9')) = (ox - 48)
            | ((ox >= ord 'a') && (ox <= ord 'z')) = (ox - 87)
            | ((ox >= ord 'A') && (ox <= ord 'Z')) = (ox - 29) 
            | otherwise = error "String not correct" where 
                ox = ord x
--READY
conv1:: Char -> Int --преобразование одного символа
conv1 m = func m where
    func x  | ((ox >= ord '0') && (ox <= ord '9')) = (ox - 48)
            | ((ox >= ord 'a') && (ox <= ord 'z')) = (ox - 87)
            | ((ox >= ord 'A') && (ox <= ord 'Z')) = (ox - 29)
            | otherwise = error "Digit not correct" where 
                ox = ord x
--READY
dECconvint:: [Int] -> Int --из DEC строки в DEC число
dECconvint [x] = x
dECconvint (x:xs) = x*(10^(length xs)) + dECconvint xs
--READY
backConv:: [Int] -> [Char] --из числовой строки в символьную 
backConv str = map func str where
    func x  | ((x >= 0) && (x <= 9)) = chr (x + 48)
            | ((x >= 10) && (x <= 35)) = chr (x + 87)
            | ((x >= 36) && (x <= 61)) = chr (x + 29) 
            | otherwise = error "String not correct"
--READY
chkbase:: Int -> Int --проверка базы
chkbase base| ((base >= 1) && (base <= 62)) = base
            | otherwise = error "Base not correct" 
--READY
checker:: Int -> [Char] -> [Char] --проверка всех значений (вылезают ли они за базу)
checker base str = filter (\x -> (if ((conv1 x) >= base) then error "String numbers are bigger then base (Incorrect string input)" else True)) str
--READY
checker1:: [Char] -> [Char] --проверка значений с базой 1
checker1 str = filter (\x -> (if ((conv1 x) /= 1) then error "String numbers are bigger then base (Incorrect string input)" else True)) str


------------------------------КОНЕЦ СЛУЖЕБНЫХ ФУНКЦИЙ--------------------------------- 


toDecimal:: Int -> [Char] -> [Char] --преобразование в DEC по базе
toDecimal 1 str = show $todec2 1 (checker1 str)
toDecimal base str = show $todec2 (chkbase base) $checker (chkbase base) str

todec2:: Int -> [Char] -> Int --вспомогательная --корректировка на фото
todec2 1 xs = length (conv xs)
todec2 base [x] = (conv1 x)
--todec2 base (x:xs) = (conv1 x)*(base^(length xs)) + todec2 base xs
todec2 base xs = (conv1 (last xs)) + base*(todec2 base (init xs))
---------------------------------------------------------------
fromDecimal:: Int -> [Char] -> [Char] --преобразование из десятичной
fromDecimal base xs = backConv $frd (chkbase base) $dECconvint $conv $checker 10 xs

--вспомогательная
frd::Int -> Int -> [Int]
frd 1 x = replicate x 1
frd base x = if x >= base then (frd base (x `div` base) ++ [x `mod` base]) else [x]
---------------------------------------------------------------
convertFromTo:: Int -> Int -> [Char] -> [Char]
convertFromTo baseF baseT str = fromDecimal baseT (toDecimal baseF str)
---------------------------------------------------------------
--поправить все невозможные ошибки