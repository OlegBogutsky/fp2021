--Лабораторная робота № 4
--Виконана Богуцьким Олегом, КН-31
--Варіант № 2

--Мета роботи: Ознайомитись з системою типiв та класiв типiв. Набути досвiду визначення
--нових типiв та класiв типiв i їх використання.

-- Фiгури на площинi. Використовуються такi фiгури, як коло (центр та ра-
-- дiус), прямокутник (координати лiвої верхньої та правої нижньої точок), три-
-- кутник (координати вершин) та мiтка — label (координати лiвої нижньої точки,
-- шрифт та рядок). Доступнi шрифти — Consolas, Lucida Console та Source Code
-- Pro. Визначне функцiї для:

--отримання списку фiгур вказаного типу;

--91. знаходження фiгури, яка має спiльнi точки з прямокутником, що охоплює вказану фiгуру;
--92. перемiщення фiгури на вказаний вектор.

--Хід роботи



--Завдання 1:


data Font = Consolas | LucidaConsole | SourceCodePro deriving Show

data Shape = Rectangle Int Int Int Int | Circle Int Int Int | Triangle Int Int Int Int Int Int | Label Int Int Font Int deriving Show

findRectangle :: [Shape] -> [Shape]
findRectangle [] = []
findRectangle (Rectangle x1 x2 x3 x4:xs) =  Rectangle x1 x2 x3 x4 : findRectangle xs
findRectangle (Circle x y radius:xs) = findRectangle xs
findRectangle (Triangle a1 b1 a2 b2 c1 c2:xs) = findRectangle xs
findRectangle (Label a b f r:xs) = findRectangle xs

findCircle :: [Shape] -> [Shape]
findCircle [] = []
findCircle (Rectangle x1 x2 x3 x4:xs) =  findCircle xs
findCircle (Circle x y radius:xs) = Circle x y radius : findCircle xs
findCircle (Triangle a1 b1 a2 b2 c1 c2:xs) = findCircle xs
findCircle (Label a b f r:xs) = findCircle xs

findTriangle :: [Shape] -> [Shape]
findTriangle [] = []
findTriangle (Rectangle x1 x2 x3 x4:xs) =  findTriangle xs
findTriangle (Circle x y radius:xs) = findTriangle xs
findTriangle (Triangle a1 b1 a2 b2 c1 c2:xs) = Triangle a1 b1 a2 b2 c1 c2 : findTriangle xs
findTriangle (Label a b f r:xs) = findTriangle xs

findLabel :: [Shape] -> [Shape]
findLabel [] = []
findLabel (Rectangle x1 x2 x3 x4:xs) =  findLabel xs
findLabel (Circle x y radius:xs) = findLabel xs
findLabel (Triangle a1 b1 a2 b2 c1 c2:xs) = findLabel xs
findLabel (Label a b f r:xs) = Label a b f r : findLabel xs

a = findRectangle [Rectangle 1 2 3 4, Circle 1 2 3, Rectangle 2 3 4 5, Triangle 4 5 6 7 8 9, Label 1 2 Consolas 2, Triangle 1 2 3 4 5 6, Circle 4 5 6, Label 2 3 LucidaConsole 4] 
--[Rectangle 1 2 3 4,Rectangle 2 3 4 5]
b = findCircle [Rectangle 1 2 3 4, Circle 1 2 3, Rectangle 2 3 4 5, Triangle 4 5 6 7 8 9, Label 1 2 Consolas 2, Triangle 1 2 3 4 5 6, Circle 4 5 6, Label 2 3 LucidaConsole 4] 
--[Circle 1 2 3,Circle 4 5 6]
c = findTriangle [Rectangle 1 2 3 4, Circle 1 2 3, Rectangle 2 3 4 5, Triangle 4 5 6 7 8 9, Label 1 2 Consolas 2, Triangle 1 2 3 4 5 6, Circle 4 5 6, Label 2 3 LucidaConsole 4] 
--[Triangle 4 5 6 7 8 9,Triangle 1 2 3 4 5 6]
d = findLabel [Rectangle 1 2 3 4, Circle 1 2 3, Rectangle 2 3 4 5, Triangle 4 5 6 7 8 9, Label 1 2 Consolas 2, Triangle 1 2 3 4 5 6, Circle 4 5 6, Label 2 3 LucidaConsole 4] 
--[Label 1 2 Consolas 2,Label 2 3 LucidaConsole 4]

--Завдання 2: 

f :: (Shape, Shape) -> Bool 
f (Rectangle x1 y1 x2 y2, Circle x y radius)
    |abs (x2-x1) /= abs (y2-y1) = False
    |x - x1 /= x2 - x = False
    |y - y1 /= y2 - y = False 
    |x2 - 2*radius /= x1 = False 
    |otherwise = True
f (Rectangle x1 y1 x2 y2, Label a b font r) = b + r == y1 && y2 == b && a >= x1 && a <= x2
f (Rectangle x1 y1 x2 y2, Triangle a1 b1 a2 b2 a3 b3) =
    a1 >= x1 && a1 <= x2 && a2 >= x1 && a2 <= x2 && a3 >= x1 && a3 <= x2 &&
    b1 <= y1 && b1 >= y2 && b2 <= y1 && b2 >= y2 && b2 <= y1 && b2 >= y2 &&
    ((a1 == x1 || a1 == x2) || (b1 == y1 || b1 == y2)) && 
    ((a2 == x1 || a2 == x2) || (b2 == y1 || b2 == y2)) &&
    ((a3 == x1 || a3 == x2) || (b3 == y1 || b3 == y2))
f (_,_) = False 

y = f (Rectangle 2 4 4 2, Circle 3 3 1) --True
z = f (Rectangle 2 4 4 2, Circle 3 2 1) --False
k1 = f (Rectangle 0 3 3 0, Label 0 0 Consolas 3) --True
p =  f (Rectangle 2 4 4 2, Label 1 2 Consolas 3) --False
p1 = f (Rectangle 0 3 3 0, Triangle 0 2 2 3 3 2) --True
p2 = f (Rectangle 0 3 3 0, Triangle 0 2 2 3 4 2) --False


--Завдання 3: 

vectorTrans :: (Shape, Int, Int) -> Shape
vectorTrans (Rectangle x1 x2 x3 x4, xs, ys) =  Rectangle (x1 + xs) (x2 + ys) (x3 + xs) (x4 + ys)
vectorTrans (Circle x y radius, xs, ys) = Circle (x + xs) (y + ys) radius
vectorTrans (Triangle a1 b1 a2 b2 a3 b3, xs, ys) = Triangle (a1 + xs) (b1 + ys) (a2 + xs) (b2 + ys) (a3 + xs) (b3 + ys)
vectorTrans (Label a b f r, xs, ys) = Label (a + xs) (b + ys) f r

e = vectorTrans(Rectangle 1 2 3 4, 2, 3) --Rectangle 3 5 5 7
k = vectorTrans(Circle 0 0 1, 1, 1) --Circle 1 1 1
g = vectorTrans(Triangle 0 0 1 1 2 2, -2, -2) --Triangle (-2) (-2) (-1) (-1) 0 0
h = vectorTrans(Label 1 1 Consolas 3, 0,0) --Label 1 1 Consolas 3

--Висновок: Ознайомились з системою типiв та класiв типiв. Набули досвiду визначення
--нових типiв та класiв типiв i їх використання.