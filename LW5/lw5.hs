
module LW3 where
import System.IO
--Лабораторная робота № 5
--Виконана Богуцьким Олегом, КН-31
--Варіант № 2

--Мета роботи: Ознайомитись з модульною органiзацiєю програм та засобами введення-
--виведення. Набути досвiду компiляцiї Haskell-програм.

--Завдання:

--Реалiзувати та скомпiлювати одну з програм, розроблених у лабора-
--торнiй роботi No 3 для Вашого варiанта з введенням даних: а) з клавiатури, б) з
--файлу та виведенням результатiв: в) на екран, г) у файл.

--Хід роботи

rev :: [Int] -> [Int]
rev [] = []
rev (x:xs) = rev xs ++ [x]

isPalindrome1 :: [Int] -> Bool
isPalindrome1 [] = True
isPalindrome1 xs = xs == rev xs

isPalindrome2 :: [Int] -> Bool
isPalindrome2 [] = True
isPalindrome2 xs = xs == reverse (xs)

merge1 :: [Int] -> [Int] -> [Int]
merge1 xs [] = xs
merge1 [] ys = ys
merge1 (x:xs) (y:ys) = if xs /= [] && ys /= [] then x : y : merge1 xs ys
else [x,y]

merge2 :: [Int] -> [Int] -> [Int]
merge2 xs [] = xs
merge2 [] ys = ys
merge2 xs ys = if tail xs /= [] && tail ys /= [] then head xs : head ys : merge2 (tail xs)  (tail ys)
else [head xs,head ys]

--Завдання 1.А: 

isPal1Console::IO()
isPal1Console = do
    putStr "Enter array\n"
    line <- getLine
    let arr = read line :: [Int]
    putStr "Result:\n"
    print (isPalindrome1 arr)

--isPal1Console
--[1,2,1]: True
--[1,2,1,1]: False


isPal2Console::IO()
isPal2Console = do
    putStr "Enter array\n"
    line <- getLine
    let arr = read line :: [Int]
    putStr "Result:\n"
    print (isPalindrome2 arr)

--isPal2Console
--[1,2,1]: True
--[1,2,1,1]: False


merge1Console :: IO()
merge1Console = do
    line1 <- getLine
    let arr1 = read line1 :: [Int]
    putStr "Enter second array\n"
    line2 <- getLine
    let arr2 = read line2 :: [Int]
    putStr "Result:\n"
    print (merge1 arr1 arr2)

--merge1Console
--[1,2,3] [1,2,3,4]: [1,1,2,2,3,3]
--[1,2,3,4] [1,2,3]: [1,1,2,2,3,3]

merge2Console :: IO()
merge2Console = do
    putStr "Enter first array\n"
    line1 <- getLine
    let arr1 = read line1 :: [Int]
    putStr "Enter second array\n"
    line2 <- getLine
    let arr2 = read line2 :: [Int]
    putStr "Result:\n"
    print (merge2 arr1 arr2)

--merge2Console
--[1,2,3] [1,2,3,4]: [1,1,2,2,3,3]
--[1,2,3,4] [1,2,3]: [1,1,2,2,3,3]


--Завдання 1.В: 


isPal1FileToConsole::IO()
isPal1FileToConsole = do
    inp <- openFile "inputIsPal.txt" ReadMode
    line <- hGetLine inp
    let arr = read line :: [Int]
    putStr (line ++ "\n")
    putStr "Result:\n"
    print (isPalindrome1 arr)
    hClose inp

--isPal1FileToConsole
--[1,2,1]: True
--[1,2,1,1]: False

isPal2FileToConsole::IO()
isPal2FileToConsole = do
    inp <- openFile "inputIsPal.txt" ReadMode
    line <- hGetLine inp
    let arr = read line :: [Int]
    putStr (line ++ "\n")
    putStr "Result:\n"
    print (isPalindrome2 arr)
    hClose inp

--isPal2FileToConsole
--[1,2,1]: True
--[1,2,1,1]: False

merge1FileToConsole::IO()
merge1FileToConsole = do
    inp <- openFile "inputMerge.txt" ReadMode
    line1 <- hGetLine inp
    let arr1 = read line1 :: [Int]
    putStr (line1 ++ "\n")
    line2 <- hGetLine inp
    let arr2 = read line2 :: [Int]
    putStr (line2 ++ "\n")
    putStr "Result:\n"
    print (merge1 arr1 arr2)

--merge1FileToConsole
--[1,2,3] [1,2,3,4]: [1,1,2,2,3,3]
--[1,2,3,4] [1,2,3]: [1,1,2,2,3,3]


merge2FileToConsole::IO()
merge2FileToConsole = do
    inp <- openFile "inputMerge.txt" ReadMode
    line1 <- hGetLine inp
    let arr1 = read line1 :: [Int]
    putStr (line1 ++ "\n")
    line2 <- hGetLine inp
    let arr2 = read line2 :: [Int]
    putStr (line2 ++ "\n")
    putStr "Result:\n"
    print (merge2 arr1 arr2)

--merge2FileToConsole
--[1,2,3] [1,2,3,4]: [1,1,2,2,3,3]
--[1,2,3,4] [1,2,3]: [1,1,2,2,3,3]

--Завдання 1.Г: 

isPal1FileToFile::IO()
isPal1FileToFile = do
    inp <- openFile "inputIsPal.txt" ReadMode
    out <- openFile "outputIsPal.txt" WriteMode
    line <- hGetLine inp
    let arr = read line :: [Int]
    hPutStr out (line ++ "\n")
    hPutStr out "Result:\n"
    hPrint out (isPalindrome1 arr)
    hClose inp
    hClose out

--isPal1FileToFile

isPal2FileToFile::IO()
isPal2FileToFile = do
    inp <- openFile "inputIsPal.txt" ReadMode
    out <- openFile "outputIsPal.txt" WriteMode
    line <- hGetLine inp
    let arr = read line :: [Int]
    hPutStr out (line ++ "\n")
    hPutStr out "Result:\n"
    hPrint out (isPalindrome2 arr)
    hClose inp
    hClose out

--isPal2FileToFile

merge1FileToFile::IO()
merge1FileToFile = do
    inp <- openFile "inputMerge.txt" ReadMode
    out <- openFile "outputMerge.txt" WriteMode
    line1 <- hGetLine inp
    let arr1 = read line1 :: [Int]
    line2 <- hGetLine inp
    let arr2 = read line2 :: [Int]
    hPutStr out (line1 ++ "\n")
    hPutStr out (line2 ++ "\n")
    hPutStr out "Result:\n"
    hPrint out (merge1 arr1 arr2)
    hClose inp
    hClose out

--merge1FileToFile

merge2FileToFile::IO()
merge2FileToFile = do
    inp <- openFile "inputMerge.txt" ReadMode
    out <- openFile "outputMerge.txt" WriteMode
    line1 <- hGetLine inp
    let arr1 = read line1 :: [Int]
    line2 <- hGetLine inp
    let arr2 = read line2 :: [Int]
    hPutStr out (line1 ++ "\n")
    hPutStr out (line2 ++ "\n")
    hPutStr out "Result:\n"
    hPrint out (merge2 arr1 arr2)
    hClose inp
    hClose out

--merge2FileToFile

--Висновок: Ознайомились з модульною органiзацiєю програм та засобами введення-
--виведення.