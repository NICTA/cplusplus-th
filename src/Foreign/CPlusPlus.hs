{-# LANGUAGE TemplateHaskell #-}
module Foreign.CPlusPlus where

import Data.List
import Data.Char
import Foreign.NM
import Language.Haskell.TH

cplusplus :: String -> FilePath -> Q Type -> Q [Dec]
cplusplus name objfile t = do
  -- we drop the last _ so we can call things reserved names.
  let nameLit = let (Just i) = findIndex (== '(') name
                    (left, lst:right) = splitAt (pred i) name
                in if lst == '_' then (left ++ right) else name
  let (x:xs) = map (\y -> if y `elem` ":<>, " then '_' else y) $ takeWhile (/= '(') name
  let hsname = mkName $ toLower x:xs
  cname <- runIO $ lookupSymbol objfile nameLit
  let cname' = case cname of
                Just v -> v
                Nothing -> error $ "symbol \"" ++ nameLit ++ "\" missing from object file"
  t' <- runQ t
  return $ [ForeignD (ImportF cCall unsafe (drop 1 cname') hsname t')]
