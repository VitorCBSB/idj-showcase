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
import qualified Routes

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
apiRoutes = [ ("api/new_user/:username/:password/:realname", with auth Routes.handleNewUser)
            , ("api/login", method POST $ with auth Routes.handleLogin)
            , ("api/logged_user", with auth Routes.handleLoggedUser)
            , ("api/pending_reviews", with auth Routes.handlePendingReviews)
            , ("api/logout", with auth Routes.handleLogout)
            , ("api/games", Routes.handleGames)
            , ("api/submit_game", Routes.handleGameSubmission)
            ]

pageRoutes :: [(BS.ByteString, Handler App App ())]
pageRoutes = [ ("assets", serveDirectory "assets")
             , ("pages", serveDirectory "pages")
             , ("", with auth Routes.handleEverything)
             ]
