{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- | This module defines our application's state type and an alias for its
-- handler monad.
module Application where

------------------------------------------------------------------------------
import qualified Data.ByteString.Char8 as BS
import Snap.Core
import Snap.Snaplet
import Snap.Snaplet.Auth.Backends.SqliteSimple
import Snap.Snaplet.SqliteSimple
import Snap.Snaplet.Session.Backends.CookieSession
import Snap.Util.FileServe (serveDirectory)

import Types
import qualified ApiRoutes
import qualified PageRoutes
import qualified Path

------------------------------------------------------------------------------

app :: SnapletInit App App
app = makeSnaplet "idj" "Server used for UnB's Game Design course showcase." Nothing $ do
    s <- nestSnaplet "sess" sess $
           initCookieSessionManager "site_key.txt" "sess" (Just 3600)
    d <- nestSnaplet "db" db sqliteInit
    a <- nestSnaplet "auth" auth $
           initSqliteAuth sess d
    addRoutes routes
    return $ App s d a

routes :: [(BS.ByteString, Handler App App ())]
routes = apiRoutes ++ pageRoutes

apiRoutes :: [(BS.ByteString, Handler App App ())]
apiRoutes = [ ("api/new_user/:username/:password/:realname", with auth ApiRoutes.handleNewUser)
            , ("api/login", method POST $ with auth ApiRoutes.handleLogin)
            , ("api/logged_user", with auth ApiRoutes.handleLoggedUser)
            , ("api/pending_reviews", with auth ApiRoutes.handlePendingReviews)
            , ("api/logout", with auth ApiRoutes.handleLogout)
            , ("api/games", ApiRoutes.handleGames)
            , ("api/submit_game", ApiRoutes.handleGameSubmission)
            ]

pageRoutes :: [(BS.ByteString, Handler App App ())]
pageRoutes = [ ("assets", serveDirectory "assets")
             , ("artifacts", serveDirectory Path.artifactDirectory)
             , ("login", PageRoutes.handleLoginPage)
             , ("games", PageRoutes.handleCatalogPage)
             , ("", with auth PageRoutes.handleEverything)
             ]
