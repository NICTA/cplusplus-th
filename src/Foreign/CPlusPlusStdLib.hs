{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances, ScopedTypeVariables, OverlappingInstances, UndecidableInstances, FunctionalDependencies #-}
module Foreign.CPlusPlusStdLib where

import Data.Int
import Data.Word
import qualified Data.ByteString as B
import Foreign.C
import Foreign.Ptr
import Foreign.Concurrent
import Foreign.StablePtr
import Foreign.CPlusPlus
import Foreign.ForeignPtr hiding (addForeignPtrFinalizer)
import Foreign.Marshal

data Std__basic_string_mem
type Std__basic_string = Ptr Std__basic_string_mem

cplusplus "haskell::fromCString(char const*, int)" "cbits/hsstring.o" [t|CString -> Int -> IO Std__basic_string|]
cplusplus "haskell::toCString(std::__1::basic_string<char, std::__1::char_traits<char>, std::__1::allocator<char> > const&)" "cbits/hsstring.o" [t|Std__basic_string -> IO CString|]
cplusplus "haskell::cstringLen(std::__1::basic_string<char, std::__1::char_traits<char>, std::__1::allocator<char> > const&)" "cbits/hsstring.o" [t|Std__basic_string -> IO Int|]
-- TODO: ffi this from the libc++?
cplusplus "haskell::deleteString(std::__1::basic_string<char, std::__1::char_traits<char>, std::__1::allocator<char> > const*)" "cbits/hsstring.o" [t|Std__basic_string -> IO ()|]

class CPlusPlusLand a {- haskell side -} b {- c++ side -} where
  to :: a -> IO b
  from :: b -> IO a

instance CPlusPlusLand Int Int where
  to = return
  from = return

instance CPlusPlusLand Int CInt where
  to = return . fromIntegral
  from = return . fromIntegral

instance CPlusPlusLand Int64 CLLong where
  to = return . fromIntegral
  from = return . fromIntegral

instance CPlusPlusLand Int32 CInt where
  to = return . fromIntegral
  from = return . fromIntegral

instance CPlusPlusLand Word32 CUInt where
  to = return . fromIntegral
  from = return . fromIntegral

instance CPlusPlusLand Bool CChar where
  to False = return 0
  to True = return 1
  from 0 = return False
  from _ = return True

instance CPlusPlusLand a b => CPlusPlusLand [a] [b] where
  to = mapM to
  from = mapM from

instance CPlusPlusLand a b => CPlusPlusLand (Maybe a) (Maybe b) where
  to (Just x) = fmap Just $ to x
  to Nothing = return Nothing
  from (Just x) = fmap Just $ from x
  from Nothing = return Nothing

instance CPlusPlusLand String (Ptr Std__basic_string_mem) where
  to x = withCStringLen x $ \(y, len) -> haskell__fromCString y len
  from x = haskell__toCString x >>= \p -> haskell__cstringLen x >>= \len -> peekCStringLen (p, len)

instance CPlusPlusLand B.ByteString (Ptr Std__basic_string_mem) where
  to x = B.useAsCStringLen x $ \(p, len) -> haskell__fromCString p len
  from x = haskell__toCString x >>= \p -> haskell__cstringLen x >>= \len -> B.packCStringLen (p, len)

retainForeign :: ForeignPtr a -> Ptr Std__basic_string_mem -> IO ()
retainForeign p v = addForeignPtrFinalizer p (haskell__deleteString v)

-- Setting / Getting maybes
setMaybePtr :: (Ptr a -> IO ()) -> (IO ()) -> Ptr a -> IO ()
setMaybePtr isSet notSet p
  | p == nullPtr = notSet
  | otherwise    = isSet p

setMaybeVal :: (a -> IO ()) -> IO () -> Maybe a -> IO ()
setMaybeVal f unset Nothing = unset
setMaybeVal f unset (Just x) = f x

getMaybeVal :: (IO a) -> (IO Bool) -> IO (Maybe a)
getMaybeVal val isSet = do
  x <- isSet
  if x then fmap Just val else return Nothing

getMaybePtr :: (IO (Ptr a)) -> (IO Bool) -> IO (Ptr a)
getMaybePtr deref isSet = do
  r <- isSet
  if r
    then deref
    else return nullPtr
