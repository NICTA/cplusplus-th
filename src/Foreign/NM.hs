{-# LANGUAGE CPP #-}
module Foreign.NM (
    lookupSymbol
#ifdef TESTING
  , readFunctions'
  , Function(..)
#endif
  ) where

import Data.Char
import Data.List
import Data.Maybe
import Numeric
import System.Process

data Function = Function {
    _addr :: Int
  , cname :: String
  , prettyName :: String
  }
  deriving (Show, Eq)

lookupSymbol :: String -> String -> IO (Maybe String)
lookupSymbol filename func = do
  functions <- readFunctions filename
  return $ fmap cname $ find ((==) func . prettyName) functions

readFunctions :: String -> IO [Function]
readFunctions filename = do
  nm <- readProcess "nm" [filename] ""
  filt <- readProcess "c++filt" [] nm
  return $ readFunctions' nm filt

readFunctions' nm filt =
  let addrLen = length $ takeWhile (/= ' ') $ head $ lines nm
      f = catMaybes . map (splitLine addrLen) . lines
      filt' = f filt
  in catMaybes $ map (\(a, v) -> lookup a filt' >>= Just . Function a v) (f nm)

-- for 64 bit, the addrLen is 16 chars
-- for 32 bit, it is 8
splitLine :: Int -> String -> Maybe (Int, String)
splitLine addrLen l =
  let (h, t) = fmap n $ splitAt addrLen l
      [(v, "")] = readHex h
      n :: String -> String
      n (' ':_:' ':x) = x
      n (' ':x) = x
      n _ = error "invalid nm output"
      eh = isSuffixOf "(.eh)" t
  in if all isHexDigit h && not eh
      then Just (v, t)
      else Nothing
