{-# LANGUAGE OverloadedStrings #-}

module PageRoutes where

import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import Snap
import Snap.Snaplet.Auth
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Html.Renderer.Utf8 as Blaze

import Types
import Query

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
                    H.title (H.toHtml ("WOAH!" :: T.Text))
                H.body $ do
                    H.p (H.toHtml (loggedText :: T.Text))

notFound :: (MonadSnap m) => m ()
notFound =
    do  modifyResponse $ setResponseStatus 404 "Not found"
        writeBuilder $ Blaze.renderHtmlBuilder $ do
            H.head $ do
                H.meta H.! A.charset "UTF-8"
                H.title (H.toHtml ("Not found" :: T.Text))
            H.body $ do
                H.p (H.toHtml ("Não encontrei nada." :: T.Text))

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
