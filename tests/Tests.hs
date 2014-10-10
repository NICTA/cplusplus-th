module Main where

import Foreign.NM

import Test.QuickCheck
import Test.QuickCheck.Monadic

main = quickCheckWith stdArgs $ once $ monadicIO $ do
  x <- run $ do
    nm <- readFile "tests/nm.linux"
    filt <- readFile "tests/filt.linux"
    return $ readFunctions' nm filt
  assert $ x == [Function {_addr = 0, cname = "GCC_except_table1", prettyName = "GCC_except_table1"},Function {_addr = 44, cname = "GCC_except_table4", prettyName = "GCC_except_table4"},Function {_addr = 448, cname = "_GLOBAL__I_a", prettyName = "global constructors keyed to a"},Function {_addr = 304, cname = "_ZN7haskell10cstringLenERKSs", prettyName = "haskell::cstringLen(std::basic_string<char, std::char_traits<char>, std::allocator<char> > const&)"},Function {_addr = 64, cname = "_ZN7haskell11fromCStringEPKci", prettyName = "haskell::fromCString(char const*, int)"},Function {_addr = 336, cname = "_ZN7haskell12deleteStringEPKSs", prettyName = "haskell::deleteString(std::basic_string<char, std::char_traits<char>, std::allocator<char> > const*)"},Function {_addr = 272, cname = "_ZN7haskell9toCStringERKSs", prettyName = "haskell::toCString(std::basic_string<char, std::char_traits<char>, std::allocator<char> > const&)"},Function {_addr = 0, cname = "_ZStL8__ioinit", prettyName = "GCC_except_table1"},Function {_addr = 0, cname = "__cxx_global_var_init", prettyName = "GCC_except_table1"}]

