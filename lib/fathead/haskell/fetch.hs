module Main where

import Data.Function (on)
import Data.List (groupBy, elemIndices)
import Data.List (isPrefixOf)
import Data.Monoid ((<>))
import Network.HTTP (simpleHTTP, getRequest, getResponseBody)
import System.Directory (listDirectory, createDirectoryIfMissing)
import System.FilePath ((</>))
import System.Process (callCommand)
import Text.JSON.String (runGetJSON, readJSArray)
import Text.JSON.Types (JSValue(JSBool, JSArray, JSString), JSString(fromJSString))
import qualified Codec.Archive.Tar as Tar
import qualified Data.Map as M


haskellUrlBase :: String
haskellUrlBase = "http://hackage.haskell.org"


getHackage :: String -> IO String
getHackage q = simpleHTTP (getRequest reqUrl) >>= getResponseBody
  where reqUrl = haskellUrlBase </> q


-- | Retrieve the list of packages with their documentation status (yes/no)
-- from packages/docs.json
hasDocs :: IO (M.Map String Bool)
hasDocs = do
  response <- getHackage "packages/docs.json"
  let json     = case runGetJSON readJSArray response of
                Left err -> error err
                Right v  -> M.fromList . fromJS $ v
  pure json
  where fromJS (JSArray xs) = fmap (\(JSArray [(JSString s),(JSBool b)]) -> (fromJSString s, b)) xs


onlyDocumented :: M.Map String Bool -> [String]
onlyDocumented = M.keys . M.filter id


onlyLatest :: [String] -> [String]
onlyLatest = fmap last . byName
  where byName = groupBy haveSamePackageName
        haveSamePackageName = ((==) `on` packageName)
        packageName = reverse . tail . snd . break (=='-') . reverse


downloadPackages :: [String] -> IO [String]
downloadPackages ps = createDirectoryIfMissing True "download" >> (sequence $ fmap (fetchPackage) ps)
  where fetchPackage p = grab >> pure outputPath
          where tarUrl = haskellUrlBase <> "/package/" <> p <> "/docs.tar"
                outputPath = "download" </> p <> ".tar"
                grab = callCommand $ concat ["wget ", tarUrl, " -P download", " -O " <> outputPath]


extractPackages :: [String] -> IO ()
extractPackages = sequence_ . fmap (Tar.extract "download")


fetchPackages :: [String] -> IO ()
fetchPackages ps = downloadPackages ps >>= extractPackages


latestPackages :: IO [String]
latestPackages = (onlyLatest . onlyDocumented) <$> hasDocs


fetchBase :: IO ()
fetchBase = basePackages >>= fetchPackages
  where basePackages = filter isBasePackage <$> latestPackages
        isBasePackage p = isPrefixOf "base" p && length (elemIndices '-' p) == 1


main :: IO ()
main = fetchBase
