--Лабораторная робота № 1
--виконана Богуцьким Олегом, КН-31


--Завдання 2.2а
f1 :: (Integer, Integer, Integer) -> Bool
f1 (x,y,z) = if ((x^2 + y^2 == z^2 || y^2 + z^2 == x^2 || x^2 + z^2 == y^2) && x > 0 && y > 0 && z > 0) then True
    else False


--Завдання 2.2б
f2 :: Integer -> Integer -> Integer -> Bool
f2 x y z = if ((x^2 + y^2 == z^2 || y^2 + z^2 == x^2 || x^2 + z^2 == y^2) && x > 0 && y > 0 && z > 0) then True
    else False
