cplusplus-th
============

`cplusplus-th` allows you to foreign import C++ functions that are
compatible with the `ccall` calling convention. It also includes
some standard library abstractions.

Example
-------

With the following C++ function in the object file `cbits/string.o`:

```c++
namespace haskell {
string* fromCString(char const* x, int length) {
  return new string(x, length);
}
}
```

We can import it into Haskell with:

```haskell
cplusplus "haskell::fromCString(char const*, int)" "cbits/string.o"
          [t|CString -> Int -> IO Std__basic_string|]
```

Building
--------

It works by looking up the symbol in the object file.
When using cabal, it is recommended you include the C++ file in
your `c-sources` and compile it in a build hook. For example:

```haskell
import Distribution.Simple
import Distribution.Simple.Setup
import Distribution.Simple.Program
import Distribution.Simple.Program.Types
import Distribution.Simple.LocalBuildInfo
import Distribution.PackageDescription

cc_flags = ["-stdlib=libc++", "-o", "cbits/string.o", "-c", "cbits/string.cc"]

main :: IO ()
main = defaultMainWithHooks simpleUserHooks {
    buildHook = buildCPlusPlus
  }

buildCPlusPlus :: PackageDescription -> LocalBuildInfo -> UserHooks -> BuildFlags -> IO ()
buildCPlusPlus pkg buildInfo hooks flags = do
  let verb = fromFlag (buildVerbosity flags)
  clang <- findProgramLocation verb "clang++"
  let clang' = case clang of
                Just x -> x
                Nothing -> error "clang++ not on path"
  runProgram verb (simpleConfiguredProgram "clang++" (FoundOnSystem clang')) cc_flags
  buildHook simpleUserHooks pkg buildInfo hooks flags
```

Standard Library
----------------

`Foreign.CPlusPlusStdLib` exports the following type class:

```haskell
class CPlusPlusLand a {- haskell side -} b {- c++ side -} where
  to :: a -> IO b
  from :: b -> IO a
```

To avoid orphan instances, it implements instances for
some numeric types, `String` and `ByteString`.

Compatability
-------------

- Static functions are simply the arguments.
- Static member functions take the object as the first argument.
- Functions via a vtable are not possible.
- Inline functions are not possible.
- instantiating templates is not possible.
