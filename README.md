cplusplus-th
============

cplusplus-th allows you to foreign import C++ functions that are
compatible with the ccall calling convention. It also includes
some standard library abstractions.

Example
-------

With the C++ function:
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

Given that the object is compiled and the object is put into `cbits/string.o`.

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

Compatability
-------------

- Static functions are simply the arguments.
- Member functions take the object as the first argument.
- Inline functions are not possible.
- Functions via a vtable are not possible.
- Some other functions are not possible.
