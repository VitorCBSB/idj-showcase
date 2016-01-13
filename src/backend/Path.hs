module Path
    ( artifactDirectory
    , artifact
    , nameToPath
    , nameToModule) where

import System.FilePath ((</>), (<.>))
import Data.List (intercalate)

artifactDirectory :: FilePath
artifactDirectory = "artifacts"

artifact :: [String] -> FilePath
artifact moduleName =
    artifactDirectory </> hyphenate moduleName <.> "js"

nameToPath :: [String] -> FilePath
nameToPath = foldl1 (</>)

nameToModule :: [String] -> String
nameToModule = intercalate "."

hyphenate :: [String] -> String
hyphenate = intercalate "-"
