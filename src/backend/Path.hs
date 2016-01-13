module Path
    ( artifactDirectory
    , artifact
    , nameToPath) where

import System.FilePath ((</>), (<.>))
import Data.List (intercalate)

artifactDirectory :: FilePath
artifactDirectory = "pages"

artifact :: [String] -> FilePath
artifact moduleName = 
    artifactDirectory </> hyphenate moduleName <.> "html"

nameToPath :: [String] -> FilePath
nameToPath = foldl1 (</>)

hyphenate :: [String] -> String
hyphenate = intercalate "-"
