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
import Data.Tree.NTree.TypeDefs (NTree)
import Network.URI (URI, parseURI)
import Data.Maybe (fromJust)


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


pagePath :: String -> FilePath
pagePath = (basePath<>)
  where basePath = "download/haddock/doc/html/"


readHaddockDocument :: String -> IOStateArrow s b XmlTree
readHaddockDocument = readDocument sysConfig . pagePath
  where sysConfig = [withInputEncoding iso8859_1, withParseHTML yes]


hasClass :: ArrowXml a => String -> a XmlTree XmlTree
hasClass c = hasAttrValue "class" (==c)


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


buildAbstract :: ArrowXml a => a b XmlTree -> a b String
buildAbstract p = (eelem "span" += p >>> normalizeText >>> writeDocumentToString [withOutputHTML, withRemoveWS yes])
                  >. (makeAbstract . concat)
  where makeAbstract = id


normalizeText :: ArrowXml a => a XmlTree XmlTree
normalizeText = processTopDown $ choiceA [ hasName "p" :-> normalizeP
                                         , hasName "pre" :-> normalizePre
                                         , this :-> this]
  where normalizeP = processChildren (changeText (unwords . lines) `when` isText)
        normalizePre = processChildren (changeText (escapeNewlines . stringTrim) `when` isText)
        escapeNewlines = concatMap (\x -> if x == '\n' then "\\n" else [x])


makeSourceLink :: (Arrow a) => String -> a String URI
makeSourceLink page = arr (base<>) >>> arr parseURIWithBase
  where base = "http://www.haskell.org/haddock/doc/html/" <> page <> "#"
        parseURIWithBase = maybe (fromJust $ parseURI base) id . parseURI


withClass :: ArrowXml cat => String -> String -> cat XmlTree XmlTree
withClass n c = hasName n >>> hasClass c


parseSections :: Int -> String -> String -> IO [Entry]
parseSections depth hType page = fmap (\(h,(a,u)) -> article h a u) <$> prs
  where contentDivs = foldr1 (//>) (replicate depth sectionDiv)
        sectionDiv = withClass "div" "section"
        headerSections         = deep (contentDivs >>> single (deep headerText)
                                        &&& single defaultAbstract
                                        &&& single (deep (sourceLink page)))
        headerText             = hasName hType /> getText >>> arr normalizeTitle
        prs                    = runX (readHaddockDocument page >>> headerSections)
        normalizeTitle         = Title . dropWhile (not . isAlpha)


parseDefinitions :: String -> IO [Entry]
parseDefinitions = parseSections 2 "h3"


parseSectionsTop :: String -> IO [Entry]
parseSectionsTop = parseSections 1 "h2"


onDl :: (ArrowXml a, ArrowList a) => a XmlTree b -> a XmlTree b' -> a XmlTree [(b, b')]
onDl f g = definitionList >>> unlistA >>> listA (f *** g)


definitionList :: (ArrowXml a, ArrowList a) => a XmlTree [(XmlTree, XmlTree)]
definitionList = listA (getChildren >>> (dt <+> dd)) >>> partitionA dt >>> arr pairs
  where pairs = uncurry zip
        dt = hasName "dt"
        dd = hasName "dd"


defaultAbstract :: IOSLA (XIOState ()) (NTree XNode) String
defaultAbstract = buildAbstract isAbstract
  where isAbstract = getChildren >>> (hasName "p") <+> (hasName "pre")


sourceLink :: String -> IOSLA (XIOState ()) XmlTree URI
sourceLink page = hasName "a"
                  >>> getAttrValue "name"
                  >>> makeSourceLink page


parseModuleAttributes :: IO [Entry]
parseModuleAttributes = fmap (\((h,u),a) -> article h a u) <$> prs
  where headerSections         = deep sectionDiv //> varList >>> onDl (deep headerText &&& deep (sourceLink "module-attributes.html")) defaultAbstract >>. concat
        sectionDiv = withClass "div" "section"
        varList = hasName "dl" >>> hasClass "variablelist"
        headerText             = deep (hasClass "literal") /> getText >>> arr normalizeTitle
        prs                    = runX (readHaddockDocument "module-attributes.html" >>> headerSections)
        normalizeTitle         = Title . dropWhile (not . isAlpha)


parseFlags :: IO [Entry]
parseFlags = concat . fmap toEntry <$> prs
  where headerSections         = deep chapterDiv //> varList >>> onDl (parseDt) defaultAbstract >>. concat
        toEntry ((es, u), a) = fmap (addEntry u a) es
          where addEntry _ _ (Right e) = e
                addEntry u a (Left h)  = article h a u
        chapterDiv = withClass "div" "chapter"
        varList = hasName "dl" >>> hasClass "variablelist"
        headerText             = hasClass "option" /> (getText >>> arr normalizeTitle)
        prs                    = runX (readHaddockDocument "invoking.html" >>> headerSections)
        normalizeTitle         = Title
        parseDt = single (deep headerText >. makeTitles) &&& single (deep (sourceLink "invoking.html"))
        makeTitles [] = []
        makeTitles (x:xs) = Left x : fmap (Right . (flip alias) x) xs


markupParsers :: [IO [Entry]]
markupParsers = [ parseDefinitions "ch03s08.html"
                , parseSectionsTop "markup.html"
                , parseDefinitions "ch03s02.html"
                , parseSectionsTop "ch03s03.html"
                , parseDefinitions "ch03s04.html"
                , parseSectionsTop "ch03s04.html"
                , parseSectionsTop "ch03s05.html"
                , parseSectionsTop "hyperlinking.html"
                ]


-- | Entries to be inserted into output file.
makeEntries :: IO [Entry]
makeEntries = fmap concat . sequence $ entries
  where entries = [parseModuleAttributes, parseFlags] <> markupParsers


main :: IO ()
main = makeEntries >>= BSZ.writeFile "output.txt"
       . encodeByNameWith encodeOptions outputHeader
