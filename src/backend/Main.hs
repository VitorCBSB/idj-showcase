{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception (SomeException, try)
import System.IO (hPutStrLn, stderr)
import System.Exit (exitFailure)
import System.FilePath ((</>), (<.>))
import System.Directory (createDirectoryIfMissing, doesFileExist)
import Control.Monad (forM, unless)
import Control.Monad.Except (runExceptT)
import GHC.Conc (setNumCapabilities, getNumProcessors)
import System.Console.CmdArgs
import Snap.Core
import Snap.Snaplet
import Snap.Snaplet.Config
import Snap.Http.Server
import Snap.Loader.Static
import Data.Text as T

import qualified Utils
import qualified Path
import Application

data ServerInput = ServerInput
    { port :: Int
    , runOnly :: Bool
    }
    deriving (Typeable, Data, Show, Eq)

serverInput :: ServerInput
serverInput = ServerInput
    { port = 8000 &= help "Set the port of the server"
    , runOnly = False &= help "Just run the server, do not compile the UI"
    }

main :: IO ()
main = do
    setNumCapabilities =<< getNumProcessors
    setupLogging

    (conf, site, cleanup) <- $(loadSnapTH [| getConf |]
                                          'getActions
                                          ["snaplets/postgresql-simple"])

    _ <- try $ httpServe conf site :: IO (Either SomeException ())
    cleanup
            
getConf :: IO (Config Snap AppConfig)
getConf = 
    do  cargs <- cmdArgs serverInput
        unless (runOnly cargs)
            compileElmFiles
        return $ setPort (port cargs) defaultConfig

getActions :: Config Snap AppConfig -> IO (Snap (), IO ())
getActions conf = do
    (msgs, site, cleanup) <- runSnaplet
        (appEnvironment =<< getOther conf) app
    hPutStrLn stderr $ T.unpack msgs
    return (site, cleanup)

compileElmFiles :: IO ()
compileElmFiles =
  do  createDirectoryIfMissing True Path.artifactDirectory
      result <-
        runExceptT $
            forM publicModules $ \name ->
                Utils.run "elm-make"
                    [ "src" </> "frontend" </> Path.nameToPath name <.> "elm"
                    , "--yes"
                    , "--output=" ++ Path.artifact name
                    ]
      case result of
        Right _ -> return ()
        Left msg ->
          do  hPutStrLn stderr msg
              exitFailure

publicModules :: [[String]]
publicModules =
    [ ["Page", "Main"]
    , ["Page", "Catalog"]
    , ["Page", "Login"]
    ]

setupLogging :: IO ()
setupLogging =
  do  createDirectoryIfMissing True "log"
      createIfMissing "log/access.log"
      createIfMissing "log/error.log"
  where
    createIfMissing path =
      do  exists <- doesFileExist path
          unless exists $ writeFile path ""

