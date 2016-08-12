module Main where

import Network.HTTP (simpleHTTP, getRequest, getResponseBody)
import System.FilePath ((</>))
import Text.JSON.String (runGetJSON, readJSArray)
import Text.JSON.Types (JSValue(JSBool, JSArray, JSString), JSString(fromJSString))
import qualified Data.Map as M
import Data.Function (on)
import Data.List (groupBy)


haskellUrlBase :: String
haskellUrlBase = "http://hackage.haskell.org"


-- | Retrieve the list of packages with their documentation status (yes/no)
-- from packages/docs.json
hasDocs :: IO (M.Map String Bool)
hasDocs = do
  response <- simpleHTTP (getRequest reqUrl) >>= getResponseBody
  let json     = case runGetJSON readJSArray response of
                Left err -> error err
                Right v  -> M.fromList . fromJS $ v
  pure json
  where reqUrl   = haskellUrlBase </> "packages" </> "docs.json"
        fromJS (JSArray xs) = fmap (\(JSArray [(JSString s),(JSBool b)]) -> (fromJSString s, b)) xs


onlyDocumented :: M.Map String Bool -> [String]
onlyDocumented = M.keys . M.filter id


onlyLatest :: [String] -> [String]
onlyLatest = fmap last . byName
  where byName = groupBy haveSamePackageName
        haveSamePackageName = ((==) `on` packageName)
        packageName = reverse . tail . snd . break (=='-') . reverse
