module FatHead.Parse
  ( onDl
  , hasClass
  , withClass
  , readHaddockDocument
  , eltUrl
  , defaultAbstract
  ) where


import Text.XML.HXT.Core
import Data.Monoid ((<>))
import Data.Tree.NTree.TypeDefs (NTree)
import Network.URI (URI, parseURI)
import Data.Maybe (fromJust)


pagePath :: String -> FilePath
pagePath = (basePath<>)
  where basePath = "download/haddock/doc/html/"


readHaddockDocument :: String -> IOStateArrow s b XmlTree
readHaddockDocument = readDocument sysConfig . pagePath
  where sysConfig = [withInputEncoding iso8859_1, withParseHTML yes]


hasClass :: ArrowXml a => String -> a XmlTree XmlTree
hasClass c = hasAttrValue "class" (==c)


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


-- | Create a source URL appropriate for use in articles from
-- the current element's name and page.
eltUrl :: String -> IOSLA (XIOState ()) XmlTree URI
eltUrl page = anchor >>> makeSourceLink page
  where anchor = hasAttr "name" >>> getAttrValue "name"
