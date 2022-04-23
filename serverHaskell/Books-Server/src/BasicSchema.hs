{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}

module BasicSchema where

import           Data.Aeson
import           Data.Aeson.Types
import           Database.Persist (Entity(..), Entity)
import           Database.Persist.Sql (fromSqlKey, toSqlKey)
import qualified Database.Persist.TH as PTH
import           Data.Text (Text)


PTH.share [PTH.mkPersist PTH.sqlSettings, PTH.mkMigrate "migrateAll"] [PTH.persistLowerCase|
  Language sql=languages
    name Text
    UniqueTitle name
    deriving Show Read Eq

  Book sql=books
    title Text
    thumbnail Text
    pages Int
    link Text
    publisher Text
    languageId LanguageId
    UniqueText title
    deriving Show Read Eq
|]

instance ToJSON (Entity Book) where
  toJSON (Entity bid book) = object $
    "id" .= (fromSqlKey bid) : bookPairs book

instance ToJSON Book where
  toJSON book = object (bookPairs book)

bookPairs :: Book -> [Pair]
bookPairs book =
  [ "title" .= bookTitle book
  , "thumbnail" .= bookThumbnail book
  , "pages" .= bookPages book
  , "link" .= bookLink book
  , "publisher" .= bookPublisher book
  , "languageId" .= bookLanguageId book
  ]

instance FromJSON (Entity Book) where
  parseJSON = withObject "Book Entity" $ \o -> do
    book <- parseBook o
    bid <- o .: "id"
    return $ Entity (toSqlKey bid) book


instance FromJSON Book where
  parseJSON = withObject "Book" parseBook
  

parseBook :: Object -> Parser Book
parseBook o = do
  uTitle <- o .: "title"
  uThumbnail <- o .: "thumbnail"
  uPages <- o .: "pages"
  uLink <- o .: "link"
  uPublisher <- o .: "publisher"
  uLanguageId <- o .: "languageId"
  return Book
    { bookTitle = uTitle
    , bookThumbnail = uThumbnail
    , bookPages = uPages
    , bookLink = uLink
    , bookPublisher = uPublisher
    , bookLanguageId = uLanguageId
    }


--------------------------------------------------------------
instance ToJSON (Entity Language) where
  toJSON (Entity lid language) = object $
    "id" .= (fromSqlKey lid) : languagePairs language

instance ToJSON Language where
  toJSON language = object (languagePairs language)

languagePairs :: Language -> [Pair]
languagePairs language =
  [ "name" .= languageName language
  ]

instance FromJSON (Entity Language) where
  parseJSON = withObject "Language Entity" $ \o -> do
    language <- parseLanguage o
    lid <- o .: "id"
    return $ Entity (toSqlKey lid) language

--
instance FromJSON Language where
  parseJSON = withObject "Language" parseLanguage

parseLanguage :: Object -> Parser Language
parseLanguage o = do
  uName <- o .: "name"
  return Language
    { languageName = uName
    }