--Лабораторная робота № 5
--Виконана Богуцьким Олегом, КН-31
--Варіант № 2

--Мета роботи: Ознайомитись з модульною органiзацiєю програм та засобами введення-
--виведення. Набути досвiду компiляцiї Haskell-програм.

--Завдання:

--Розробитти простий веб-застосунок засобами фреймворка Yesod з
--функціональністю, визначеною у лабораторнiй роботi No 3 для Вашого
--варiанта. Передбачається використання віджетів та
--форм. Підготувати презентацію та доповідь (до 5 хв.) про розроблений веб-
--застосунок.

--Хід роботи


{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Pal where

import Import

getPalR :: Handler Html
getPalR = do
    defaultLayout $ do
        setTitle"getPal"
        $(widgetFile "pal")
        

postPalR :: Handler Html 
postPalR = do 
    postedText <- runInputPost $ ireq textField "content"
    defaultLayout $ do
        setTitle"postPal"
        $(widgetFile "posted")

isPalindrome :: Text -> Bool
isPalindrome xs = xs == reverse (xs)

--Через термінал зробити активною директорію з проектом та ввести команду stack exec -- yesod devel
--Зайти на сайт localhost:3000/pal

--Висновок: Ознайомились з модульною органiзацiєю програм та засобами 
--створення веб застосунків на основі фреймоворку Yesod.