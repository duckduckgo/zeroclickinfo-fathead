{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

import Data.Char (ord)
import Data.Csv
import Data.String (IsString)
import qualified Data.Text as DT
import Data.Text (Text)
import qualified Data.ByteString.Lazy.Char8 as BSZ


data EntryType = EntryArticle
               | EntryDisambiguation
               | EntryRedirect


-- | The type of entry in the DB.
instance ToField EntryType where
  toField EntryArticle = "A"
  toField EntryDisambiguation = "D"
  toField EntryRedirect = "R"


newtype FieldText = FT { getFieldText :: Text }
  deriving (Show, Eq, IsString)


instance ToField FieldText where
  toField (FT {getFieldText = t}) = toField t


type Disambugation = Text


newtype Categories = Categories { getCategories :: [Text] }


instance ToField Categories where
  toField (Categories { getCategories = cs }) = toField (DT.unlines cs)


data Entry = Entry
  { entryTitle :: !FieldText
  , entryType  :: EntryType
  , entryAlias :: Maybe FieldText
  , entryCategories :: Maybe Categories
  , entryDisambiguation :: Maybe Disambugation
  , entryAbstract :: !FieldText
  , entryUrl :: !FieldText
  }


emptyField :: Field
emptyField = toField ("" :: FieldText)


instance ToRecord Entry where
  toRecord (Entry { entryTitle = title
                  , entryType  = eType
                  , entryAlias = alias
                  , entryCategories = categories
                  , entryDisambiguation = disambiguation
                  , entryAbstract = abstract
                  , entryUrl = url
                  }) =
    record [toField title, toField eType, toField alias, emptyField, toField categories
                , emptyField, emptyField, emptyField, emptyField, toField disambiguation
                , emptyField, toField abstract, toField url]


encodeOptions :: EncodeOptions
encodeOptions = defaultEncodeOptions { encDelimiter = tab
                                     , encIncludeHeader = True
                                     , encUseCrLf = False
                                     }
  where tab = fromIntegral . ord $ '\t'
