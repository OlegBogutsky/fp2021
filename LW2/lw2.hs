import Data.List
--Лабораторная робота № 2
--Виконана Богуцьким Олегом, КН-31
--Варіант № 2

--Мета роботи: Набути досвiду визначення рекурсивних функцiй, використання механiзму
--зiставлення зi зразком i роботи з кортежами та списками.

--Завдання 1: Знайти передостаннiй елемент списку.
--Завдання 2: Циклiчний лiвий зсув списку на n позицiй.

--ХІД РОБОТИ

--Завдання 1.А
prelast1a :: [a] -> [a]
prelast1a [] = []
prelast1a [x1] = []
prelast1a [x1,x2] = [x1]
prelast1a (x1:xs) = prelast1a xs

--Завдання 1.Б
prelast1b :: [a] -> [a]
prelast1b [] = []
prelast1b [x1] = []
prelast1b [x1,x2] = [x1]
prelast1b (xs) = prelast1b (tail xs)


--Завдання 2.А
leftmove2a :: ([a],Int) -> ([a],Int)
leftmove2a ([],a) = ([],0)
leftmove2a ([x1],a) = ([x1],0)
leftmove2a (xs,0) = (xs,0)
leftmove2a ((x1:xs),a) = leftmove2a (xs ++ [x1],a-1)

--Завдання 2.Б
leftmove2b :: ([a],Int) -> ([a],Int)
leftmove2b ([],a) = ([],0)
leftmove2b ([x1],a) = ([x1],0)
leftmove2b (xs,0) = (xs,0)
leftmove2b (xs,a) = leftmove2b (tail xs ++ [head xs],a-1)

--Висновок:Набули досвiду визначення рекурсивних функцiй, використання механiзму
--зiставлення зi зразком i роботи з кортежами та списками.