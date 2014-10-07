module Foreign.NM where

import Data.Char
import Data.List
import Data.Maybe
import Numeric
import System.Process

data Function = Function {
    addr :: Int
  , cname :: String
  , prettyName :: String
  }
  deriving (Show)

lookupSymbol :: String -> String -> IO (Maybe String)
lookupSymbol filename func = do
  functions <- readFunctions filename
  return $ fmap cname $ find ((==) func . prettyName) functions

readFunctions :: String -> IO [Function]
readFunctions filename = do
  nm <- readProcess "nm" [filename] ""
  filt <- readProcess "c++filt" [] nm
  let f = catMaybes . map splitLine . lines
      nm'   = f nm
      filt' = f filt
  return $ catMaybes $ map (\(a, v) -> lookup a filt' >>= Just . Function a v) nm'

splitLine :: String -> Maybe (Int, String)
splitLine x =
  let (h, t) = fmap n $ splitAt 16 x
      [(v, "")] = readHex h
      n :: String -> String
      n (' ':_:' ':x) = x
      n (' ':x) = x
      eh = isSuffixOf "(.eh)" t
  in if all isHexDigit h && not eh
      then Just (v, t)
      else Nothing