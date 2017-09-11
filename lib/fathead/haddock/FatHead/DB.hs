{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module FatHead.DB
  ( Entry
  , Title(..)
  , alias
  , article
  , writeOutput
  ) where


import Data.Char (ord)
import Data.Csv
import qualified Data.Text as DT
import qualified Data.ByteString.Lazy as BSZ
import Data.Text (Text)
import Network.URI (URI)


data EntryType = EntryArticle
               | EntryDisambiguation
               | EntryRedirect


-- | The type of entry in the DB.
instance ToField EntryType where
  toField EntryArticle = "A"
  toField EntryDisambiguation = "D"
  toField EntryRedirect = "R"


type FieldText = String
newtype Title = Title { getTitle :: String } deriving (ToField)
type Disambugation = Text
type Abstract = String


newtype Categories = Categories { getCategories :: [Text] }


instance ToField Categories where
  toField (Categories { getCategories = cs }) = toField (DT.unlines cs)


instance ToField URI where
  toField = toField . show


data Entry = Entry
  { entryTitle :: !Title
  , entryType  :: EntryType
  , entryAlias :: Maybe Title
  , entryCategories :: Maybe Categories
  , entryDisambiguation :: Maybe Disambugation
  , entryAbstract :: Maybe Abstract
  , entryUrl :: Maybe URI
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
                 ] ++ empties [ "null1", "null2", "null3", "external_links"
                              , "see_also", "images" ])
    where empties = fmap (.= emptyField)


encodeOptions :: EncodeOptions
encodeOptions = defaultEncodeOptions { encDelimiter = tab
                                     , encIncludeHeader = True
                                     , encUseCrLf = False
                                     , encQuoting = QuoteNone
                                     }
  where tab = fromIntegral . ord $ '\t'


outputHeader :: Header
outputHeader = header [ "title" , "type", "redirect", "null1"
                      , "categories", "null2", "see_also", "null3"
                      , "external_links", "disambiguation"
                      , "images", "abstract", "source_url"
                      ]


article :: Title -> Abstract -> URI -> Entry
article t a u = Entry { entryTitle =  t
                    , entryType  = EntryArticle
                    , entryAlias = Nothing
                    , entryCategories = Nothing
                    , entryDisambiguation = Nothing
                    , entryAbstract = Just a
                    , entryUrl = Just u
                    }


alias :: Title -> Title -> Entry
alias orig new = Entry { entryTitle = new
                       , entryType  = EntryRedirect
                       , entryAlias = Just orig
                       , entryCategories = Nothing
                       , entryDisambiguation = Nothing
                       , entryAbstract = Nothing
                       , entryUrl = Nothing
                       }


writeOutput :: [Entry] -> IO ()
writeOutput = BSZ.writeFile "output.txt"
              . encodeByNameWith encodeOptions outputHeader
