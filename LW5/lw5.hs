
module LW3 where
--Лабораторная робота № 3
--Виконана Богуцьким Олегом, КН-31
--Варіант № 2

--Мета роботи: Набути досвiду визначення та використання функцiй вищого порядку.

--Визначте вказанi функцiї в кожному з завдань: а) без застосування, б) з
--застосуванням вбудованих функцiй вищого порядку.

--Завдання 1: Чи є список палiндромом?
--Завдання 2: Об’єднання зi змiшуванням двох спискiв довжиною n1 та n2. Ви-
--хiдний список має довжину 2 ∗ n, де n = min(n1, n2).

--Хід роботи



--Завдання 1.А:

rev :: [Int] -> [Int]
rev [] = []
rev (x:xs) = rev xs ++ [x]

isPalindrome1 :: [Int] -> Bool
isPalindrome1 [] = True
isPalindrome1 xs = xs == rev xs

isPal = do
    putStr "Enter array\n"
    arr <- getLine
    putStr isPalindrome1(arr)
c = isPalindrome1 [1,2,1] --True
d = isPalindrome1 [1,2,2] --False



--Завдання 1.Б:

isPalindrome2 :: [Int] -> Bool
isPalindrome2 [] = True
isPalindrome2 xs = xs == reverse (xs)

a = isPalindrome2 [1,2,1] --True
b = isPalindrome2 [1,2,2] --False

--Завдання 2.А:

merge1 :: [Int] -> [Int] -> [Int]
merge1 xs [] = xs
merge1 [] ys = ys
merge1 (x:xs) (y:ys) = if xs /= [] && ys /= [] then x : y : merge1 xs ys
else [x,y]

e = merge1 [1,2,3] [1,2,3] --[1,1,2,2,3,3]
f = merge1 [1,2,3,4] [1,2,3,4,5] --[1,1,2,2,3,3,4,4]
g = merge1 [1,2,3,4,5,5] [1,2,3,4,5] --[1,1,2,2,3,3,4,4,5,5]

--Завдання 2.Б:

merge2 :: [Int] -> [Int] -> [Int]
merge2 xs [] = xs
merge2 [] ys = ys
merge2 xs ys = if tail xs /= [] && tail ys /= [] then head xs : head ys : merge2 (tail xs)  (tail ys)
else [head xs,head ys]

m = merge2 [1,2,3] [1,2,3] --[1,1,2,2,3,3]
n = merge2 [1,2,3,4] [1,2,3,4,5] --[1,1,2,2,3,3,4,4]
o = merge2 [1,2,3,4,5,5] [1,2,3,4,5] --[1,1,2,2,3,3,4,4,5,5]

--Висновок: Набули досвiду визначення та використання функцiй вищого порядку.