{-# LANGUAGE FlexibleContexts #-}

module Utils 
    (run) where

import Control.Monad.Except (MonadError, MonadIO, liftIO, throwError)
import Data.List (intercalate)
import System.Exit (ExitCode(ExitSuccess, ExitFailure))
import System.Process (readProcessWithExitCode)

data CommandError
    = MissingExe String
    | CommandFailed String String

run :: (MonadError String m, MonadIO m) => String -> [String] -> m String
run command args =
    do  result <- liftIO (unwrappedRun command args)
        case result of
            Right out ->
                return out

            Left err ->
                throwError (context (message err))
    where
        context msg =
            "failure when running: " ++ unwords (command : args) ++ "\n" ++ msg

        message err =
            case err of
                CommandFailed stderr stdout ->
                    stdout ++ stderr

                MissingExe msg ->
                    msg

unwrappedRun :: String -> [String] -> IO (Either CommandError String)
unwrappedRun command args =
    do  (exitCode, stdout, stderr) <- readProcessWithExitCode command args ""
        return $
            case exitCode of
                ExitSuccess ->
                    Right stdout

                ExitFailure code
                    | code == 127 -> Left (missingExe command) -- UNIX
                    | code == 9009 -> Left (missingExe command) -- Windows
                    | otherwise -> Left (CommandFailed stdout stderr)

missingExe :: String -> CommandError
missingExe command =
  MissingExe $
    "Could not find command `" ++ command ++ "`. Do you have it installed?\n\
    \    Can it be run from anywhere? Is it on your PATH?"

