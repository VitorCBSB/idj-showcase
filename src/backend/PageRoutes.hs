{-# LANGUAGE OverloadedStrings #-}

module PageRoutes where

import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import Snap
import Snap.Snaplet.Auth
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Html.Renderer.Utf8 as Blaze

import Types
import Query
import qualified Path

handleEverything :: Handler App (AuthManager App) ()
handleEverything =
    do  loggedUser <- currentUser
        loggedText <- case loggedUser of
                Just user -> case userId user of
                    Just uid -> do  userRealName <- withTop db $ getUserRealName uid
                                    return $ T.concat ["HELLO MR(S). ", userRealName, "!"]
                    Nothing -> return "Where's your id...?"
                Nothing -> return "HELLO! PLEASE LOGIN!"
        writeBuilder $ Blaze.renderHtmlBuilder $ H.docTypeHtml $
            do  H.head $ do
                    H.meta H.! A.charset "UTF-8"
                    favicon
                    H.title (H.toHtml ("WOAH!" :: T.Text))
                H.body $ do
                    H.p (H.toHtml (loggedText :: T.Text))

handleLoginPage :: (MonadSnap m) => m ()
handleLoginPage = servePage ["Page", "Login"]

handleCatalogPage :: (MonadSnap m) => m ()
handleCatalogPage = servePage ["Page", "Catalog"]

notFound :: (MonadSnap m) => m ()
notFound =
    do  modifyResponse $ setResponseStatus 404 "Not found"
        writeBuilder $ Blaze.renderHtmlBuilder $ do
            H.head $ do
                H.meta H.! A.charset "UTF-8"
                H.title (H.toHtml ("Not found" :: T.Text))
            H.body $ do
                H.p (H.toHtml ("Não encontrei nada." :: T.Text))

servePage :: (MonadSnap m) => [String] -> m ()
servePage moduleName =
    writeBuilder $ Blaze.renderHtmlBuilder $
        makeHtml moduleName

makeHtml :: [String] -> H.Html
makeHtml moduleName =
    H.docTypeHtml $ do
        H.head $ do
            H.meta ! charset "UTF-8"
            favicon
            H.link ! rel "stylesheet" ! href (toValue ("/assets/style.css" :: String))
            H.script ! src (toValue $ "/" ++ Path.artifact moduleName) $ ""
        H.body $
            script $ preEscapedToMarkup $
                "\nElm.fullscreen(Elm." ++ Path.nameToModule moduleName ++ ")\n"

-- HELPERS

favicon :: H.Html
favicon =
  H.link
    ! A.rel "shortcut icon"
    ! A.size "16x16, 32x32, 48x48, 64x64, 128x128, 256x256"
    ! A.href "/assets/favicon.ico"

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
