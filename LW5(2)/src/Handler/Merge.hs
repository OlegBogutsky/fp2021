{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Merge where

import Import
import Data.Text as T

getMergeR :: Handler Html
getMergeR = do
    defaultLayout $ do
        setTitle"getMerge"
        $(widgetFile "merge")
        

postMergeR :: Handler Html 
postMergeR = do 
    postedText1 <- runInputPost $ ireq textField "content1"
    postedText2 <- runInputPost $ ireq textField "content2"
    defaultLayout $ do
        setTitle"postMerge"
        $(widgetFile "postedMerge")


merge2 :: [Text] -> Text
merge2 xs = T.concat ( T.transpose (xs))
