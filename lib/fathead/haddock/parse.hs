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


instance ToNamedRecord Entry where
  toNamedRecord (Entry { entryTitle          = title
                       , entryType           = eType
                       , entryAlias          = alias
                       , entryCategories     = categories
                       , entryDisambiguation = disambiguation
                       , entryAbstract       = abstract
                       , entryUrl            = url }) =
    namedRecord ([ "title"          .= toField title
                 , "type"           .= toField eType
                 , "redirect"       .= toField alias
                 , "categories"     .= toField categories
                 , "disambiguation" .= toField disambiguation
                 , "abstract"       .= toField abstract
                 , "source_url"     .= toField url
                 ] ++ empties [ "null1", "null2", "null3", "null4"
                              , "see_also", "external_links", "images" ])
    where empties = fmap (.= emptyField)


encodeOptions :: EncodeOptions
encodeOptions = defaultEncodeOptions { encDelimiter = tab
                                     , encIncludeHeader = True
                                     , encUseCrLf = False
                                     }
  where tab = fromIntegral . ord $ '\t'


outputHeader :: Header
outputHeader = header [ "title" , "type", "redirect", "null1"
                      , "categories", "null2", "see_also", "null3"
                      , "null4", "external_links", "disambiguation"
                      , "images", "abstract", "source_url"
                      ]


-- | Entries to be inserted into output file.
entries :: [Entry]
entries = []


main :: IO ()
main = BSZ.writeFile "output.txt" $
  encodeByNameWith encodeOptions outputHeader entries
