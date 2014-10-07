import Distribution.Simple
import Distribution.Simple.Setup
import Distribution.Simple.Program
import Distribution.Simple.Program.Types
import Distribution.Simple.LocalBuildInfo
import Distribution.PackageDescription

cc_flags = ["-stdlib=libc++", "-o", "cbits/hsstring.o", "-c", "cbits/hsstring.cc"]

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
