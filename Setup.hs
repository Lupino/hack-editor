import           Control.Monad                      (forM_, (<=<))
import           Data.Default                       (def)
import           Data.List                          (find)
import           Data.List.Split                    (splitOn)
import           Data.Maybe                         (fromMaybe)
import qualified Data.Text                          as T (pack, strip, unpack)
import           System.Directory                   (doesFileExist)
import           System.FilePath                    ((<.>), (</>))

import           Distribution.PackageDescription    (PackageDescription,
                                                     customFieldsPD)
import           Distribution.Simple                (Args, PackageDB (SpecificPackageDB),
                                                     defaultMainWithHooks,
                                                     postBuild, simpleUserHooks)
import           Distribution.Simple.LocalBuildInfo (LocalBuildInfo,
                                                     withPackageDB)
import           Distribution.Simple.Setup          (BuildFlags)
import           Fay                                (Config (..), addConfigDirectoryIncludePaths,
                                                     addConfigPackages,
                                                     compileFromTo)
import           Safe                               (readMay)

-- | Compile code
build :: PackageDescription -> Maybe FilePath -> IO ()
build packageDesc pkgDb = do
  let packages     = listField_ "x-fay-packages"       packageDesc
      roots        = listField_ "x-fay-root-modules"   packageDesc
      includePaths = listField_ "x-fay-include-paths"  packageDesc
      sourceDir    = field_     "x-fay-source-dir"     packageDesc
      outputDir    = field_     "x-fay-output-dir"     packageDesc
      stricts      = listField_ "x-fay-strict-modules" packageDesc
      lib          = readField  "x-fay-library" False  packageDesc

  forM_ (zip roots [(1::Int)..]) $ \(name, i) -> do
    let candidate = sourceDir </> name <.> "hs"
        out       = outputDir </> name <.> "js"
    exists <- doesFileExist candidate
    if exists
      then do
        putStrLn $ "fay: [" ++ show i ++ " of " ++ show (length roots) ++ "] Compiling " ++ name ++ " ( " ++ candidate ++ ", " ++ out ++ " )"
        compileFromTo (fayConfig pkgDb packages sourceDir includePaths stricts lib) candidate (Just out)
      else
        error $ "fay-builder: Could not find " ++ candidate

-- | Try to read a comma separated field
listField :: String -> PackageDescription -> Maybe [String]
listField key = fmap (map strip . splitOn ",") . field key

-- | Read the value of a comma separated field, gives an empty list if the field is not present.
listField_ :: String -> PackageDescription -> [String]
listField_ fn = fromMaybe [] . listField fn

-- | Try to read a field's value
field :: String -> PackageDescription -> Maybe String
field key = fmap strip . lookup key . customFieldsPD

-- | Force reading of a field, fails if it doesn't exist
field_ :: String -> PackageDescription -> String
field_ key = fromMaybe (error $ key ++ "is  missing") . field key

readField :: Read a => String -> a -> PackageDescription -> a
readField key d = fromMaybe d . (readMay <=< field key)

-- | Default config, TODO make this optional
fayConfig :: Maybe FilePath -> [String] -> FilePath -> [FilePath] -> [String] -> Bool -> Config
fayConfig pkgDb packages dir includePs stricts lib =
    addConfigDirectoryIncludePaths (dir : includePs)
  . addConfigPackages              packages
  $ def { configWall        = True
        , configPrettyPrint = True
        , configPackageConf = pkgDb
        , configStrict      = stricts
        , configLibrary     = lib
        , configGClosure    = True
        , configOptimize    = True
        }

-- | Default build hook for your Setup.hs
defaultFayHook :: IO ()
defaultFayHook = defaultMainWithHooks simpleUserHooks { postBuild = postBuildHook }

-- | Default post build hook for your Setup.hs
postBuildHook :: Args -> BuildFlags -> PackageDescription -> LocalBuildInfo -> IO ()
postBuildHook _ _ packageDesc localBuildInfo = do
  putStrLn "Building Fay client ..."
  build packageDesc  (findSpecificPackageDb localBuildInfo)
  putStrLn "Finished building Fay client"
  where
    findSpecificPackageDb =
        fmap (\(SpecificPackageDB p) -> p)
      . find (\db -> case db of
          SpecificPackageDB{} -> True
          _                   -> False)
      . withPackageDB

-- | Strip leading and trailing whitespace
strip :: String -> String
strip = T.unpack . T.strip . T.pack

main = defaultFayHook
