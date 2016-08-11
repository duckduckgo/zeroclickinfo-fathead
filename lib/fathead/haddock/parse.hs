module Main where

import Data.Char (isAlpha)
import Text.XML.HXT.Core
import Data.Monoid ((<>))
import Data.Tree.NTree.TypeDefs (NTree)

import FatHead.DB
import FatHead.Parse


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
        toEntry (([], _), _) = []
        toEntry (((t:ts), u), a) = article t a u : fmap (alias t) ts
        chapterDiv = withClass "div" "chapter"
        varList    = withClass "dl" "variablelist"
        headerText             = hasClass "option" /> (getText >>> arr normalizeTitle)
        prs                    = runX (readHaddockDocument "invoking.html" >>> headerSections)
        normalizeTitle         = Title . normalizeWhitespace
        parseDt = single (listA $ fullTitle <+> deep headerText) &&& single (deep (sourceLink "invoking.html"))
        fullTitle = deep getText >. arr (normalizeTitle . unwords . lines . concat)
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
main = makeEntries >>= writeOutput
