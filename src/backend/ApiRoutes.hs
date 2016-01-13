{-# LANGUAGE OverloadedStrings #-}

module ApiRoutes where

import Control.Applicative ((<|>))
import qualified Data.Aeson as Json
import qualified Data.ByteString.Char8 as BS
import Data.Maybe (listToMaybe)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as Enc
import Snap
import Snap.Snaplet.Auth
import Snap.Snaplet.SqliteSimple
import Text.Read (readMaybe)

import Types
import Query

handleNewUser :: Handler App (AuthManager App) ()
handleNewUser =
    do  username <- getParameter "username" $ Right . T.pack
        password <- getParameter "password" $ Right . BS.pack
        realName <- getParameter "realname" $ Right . T.pack
        createdUser <- createUser username password
        case createdUser of
            Right user -> case userId user of
                Just uid -> withTop db $ addExtraUserInfo uid realName
                Nothing -> return ()
            Left _ -> return ()
        redirect "/"

handleLogin :: Handler App (AuthManager App) ()
handleLogin =
    do  req <- getRequest
        let postParams = rqPostParams req
        username <- maybe
                        (httpError 400 "Error with parameter username.")
                        (return . Enc.decodeUtf8)
                        (M.lookup "username" postParams >>= listToMaybe)
        password <- maybe
                        (httpError 400 "Error with parameter password.")
                        (return . ClearText)
                        (M.lookup "password" postParams >>= listToMaybe)
        authResult <- loginByUsername username password False
        case authResult of
            Right _ -> writeLBS $ Json.encode (Nothing :: Maybe T.Text)
            Left _ -> writeLBS $ Json.encode (Just ("Nome de usuario ou senha incorretos." :: T.Text))

handleLoggedUser :: Handler App (AuthManager App) ()
handleLoggedUser =
    do  loggedUser <- currentUser
        case loggedUser of
            Nothing -> writeLBS $ Json.encode (Nothing :: Maybe T.Text)
            Just authUser -> case userId authUser of
                Nothing -> httpError 500 "Attempted to retrieve user without id."
                Just uid -> do  userRealName <- withTop db $ getUserRealName uid
                                writeLBS $ Json.encode userRealName

handleLogout :: Handler App (AuthManager App) ()
handleLogout =
    logout >> redirect "/"

handleDestroyUser :: Handler App (AuthManager App) ()
handleDestroyUser =
    do  maybeLoggedUser <- currentUser
        case maybeLoggedUser of
            Just user -> do case userId user of
                                Just uid -> withTop db $ deleteExtraUserInfo uid
                                Nothing -> return()
                            destroyUser user
            Nothing -> return ()
        redirect "/"

handleGames :: Handler App App ()
handleGames =
    ifTop (with db $ serveGames NoFilter)
    <|> route [(":year", with db getFilteredGames)]
    <|> redirect "/"

serveGames :: (HasSqlite m, MonadSnap m) => GamesFilter -> m ()
serveGames filter =
    do  allGames <- getGamesFromDB filter
        writeLBS $ Json.encode allGames

getFilteredGames :: (HasSqlite m, MonadSnap m) => m ()
getFilteredGames =
    do  yearParam <- getParameter "year" Right
        case readMaybe yearParam of
            Just year -> serveGames (ByYear year)
            Nothing -> redirect "/"

handleGameSubmission :: (MonadSnap m) => m ()
handleGameSubmission = writeText "Submissão de jogos."

handlePendingReviews :: Handler App (AuthManager App) ()
handlePendingReviews =
    requireUser auth
        (writeLBS $ Json.encode (0 :: Int))
        (do pendingReviews <- withTop db getPendingReviews
            writeLBS $ Json.encode pendingReviews)

-- HELPERS

getParameter :: (MonadSnap m) => BS.ByteString -> (String -> Either String a) -> m a
getParameter param fromString =
  do  maybeValue <- getParam param
      let notFoundMsg = "could not find parameter named " ++ BS.unpack param
      let eitherString = maybe (Left notFoundMsg) (Right . BS.unpack) maybeValue
      case fromString =<< eitherString of
        Right value ->
            return value

        Left problem ->
            httpError 400 $ BS.concat [ "problem with parameter '", param, "': ", BS.pack problem ]

httpError :: (MonadSnap m) => Int -> BS.ByteString -> m a
httpError code msg = do
  modifyResponse $ setResponseCode code
  writeBS msg
  finishWith =<< getResponse
