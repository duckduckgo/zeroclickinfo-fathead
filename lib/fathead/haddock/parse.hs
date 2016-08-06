{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

import Data.Char (ord, isAlpha)
import Data.Csv
import Data.String (IsString)
import qualified Data.Text as DT
import Data.Text (Text)
import qualified Data.ByteString.Lazy.Char8 as BSZ
import Text.XML.HXT.Core
import Data.Monoid ((<>))


data EntryType = EntryArticle
               | EntryDisambiguation
               | EntryRedirect


-- | The type of entry in the DB.
instance ToField EntryType where
  toField EntryArticle = "A"
  toField EntryDisambiguation = "D"
  toField EntryRedirect = "R"


type FieldText = String
type Title = FieldText
type Disambugation = Text
type Abstract = String


newtype Categories = Categories { getCategories :: [Text] }


instance ToField Categories where
  toField (Categories { getCategories = cs }) = toField (DT.unlines cs)


data Entry = Entry
  { entryTitle :: !Title
  , entryType  :: EntryType
  , entryAlias :: Maybe FieldText
  , entryCategories :: Maybe Categories
  , entryDisambiguation :: Maybe Disambugation
  , entryAbstract :: !Abstract
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
                                     , encQuoting = QuoteNone
                                     }
  where tab = fromIntegral . ord $ '\t'


outputHeader :: Header
outputHeader = header [ "title" , "type", "redirect", "null1"
                      , "categories", "null2", "see_also", "null3"
                      , "null4", "external_links", "disambiguation"
                      , "images", "abstract", "source_url"
                      ]


pagePath :: String -> FilePath
pagePath = (basePath<>)
  where basePath = "download/haddock/doc/html/"


readHaddockDocument :: String -> IOStateArrow s b XmlTree
readHaddockDocument = readDocument sysConfig . pagePath
  where sysConfig = [withInputEncoding iso8859_1, withParseHTML yes]


hasClass :: ArrowXml a => String -> a XmlTree XmlTree
hasClass c = hasAttrValue "class" (==c)


article :: Title -> Abstract -> Entry
article t a = Entry { entryTitle =  t
                    , entryType  = EntryArticle
                    , entryAlias = Nothing
                    , entryCategories = Nothing
                    , entryDisambiguation = Nothing
                    , entryAbstract = a
                    , entryUrl = "PLACEHOLDER"
                    }


makeAbstract :: String -> Abstract
makeAbstract = unwords . lines


parseMarkup :: IO [Entry]
parseMarkup = fmap (uncurry article) <$> (fmap . fmap) (mapTuple (normalizeTitle, makeAbstract)) prs
  where divSections            = hasName "div" `guards` hasClass "section"
        headerSections         = deep (divSections `guards`
                                 ( deep headerText &&&
                                   ( getChildren >>> hasName "p" >>>
                                     writeDocumentToString [withOutputHTML, withRemoveWS yes])))
        headerText             = hasName "h3" /> getText
        prs                    = runX (readHaddockDocument "ch03s08.html" >>> headerSections)
        mapTuple (f, g) (x, y) = (f x, g y)
        normalizeTitle         = dropWhile (not . isAlpha)


-- | Entries to be inserted into output file.
makeEntries :: IO [Entry]
makeEntries = parseMarkup


main :: IO ()
main = makeEntries >>= BSZ.writeFile "output.txt"
       . encodeByNameWith encodeOptions outputHeader
