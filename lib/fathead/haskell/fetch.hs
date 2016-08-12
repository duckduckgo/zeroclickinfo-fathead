module Main where

import Network.HTTP (simpleHTTP, getRequest, getResponseBody)
import System.FilePath ((</>))
import Text.JSON.String (runGetJSON, readJSArray)
import Text.JSON.Types (JSValue(JSBool, JSArray, JSString), JSString(fromJSString))
import qualified Data.Map as M
import Data.Function (on)
import Data.List (groupBy, elemIndices)
import Data.Monoid ((<>))
import qualified Codec.Archive.Tar as Tar
import Data.List (isPrefixOf)
import System.Process (runCommand)
import System.Directory (listDirectory, createDirectoryIfMissing)


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


downloadPackages :: [String] -> IO ()
downloadPackages ps = createDirectoryIfMissing True "download" >> (sequence_ $ fmap (fetchPackage) ps)
  where fetchPackage p = runCommand $ "wget " <> tarUrl <> " -P download -O " <> outputPath
          where tarUrl = haskellUrlBase <> "/package/" <> p <> "/docs.tar"
                outputPath = "download" </> p <> ".tar"


extractPackages :: [String] -> IO ()
extractPackages = sequence_ . fmap (Tar.extract "download" . ("download"</>))


fetchPackages :: [String] -> IO ()
fetchPackages ps = downloadPackages ps >> extractPackages ps


latestPackages :: IO [String]
latestPackages = (onlyLatest . onlyDocumented) <$> hasDocs


fetchBase :: IO ()
fetchBase = basePackages >>= fetchPackages
  where basePackages = filter isBasePackage <$> latestPackages
        isBasePackage p = isPrefixOf "base" p && length (elemIndices '-' p) == 1


main :: IO ()
main = fetchBase
