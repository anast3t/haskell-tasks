sqroot:: Double -> Double -> Double -> (Double, Double)
sqroot 0 0 0 = (0, 0)
sqroot 0 0 _ = error "Корней нет"
sqroot 0 b c = ((-c)/b, (-c)/b) 
sqroot a b c = if d < 0 then error "Нет действительных корней" else (x1,x2) where
    x1 = ((-b) + sqd) / (2*a)
    x2 = ((-b) - sqd) / (2*a) 
    d = (b*b) - (4 * a * c)
    sqd = sqrt (d) 
