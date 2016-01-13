{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Types where

import Control.Lens
import Data.Aeson
import qualified Data.Text as T
import GHC.Generics
import Snap.Snaplet
import Snap.Snaplet.Auth
import Snap.Snaplet.Session
import Snap.Snaplet.SqliteSimple

data App = App
    { _sess :: Snaplet SessionManager
    , _db :: Snaplet Sqlite
    , _auth :: Snaplet (AuthManager App)
    }
makeLenses ''App

data Category =
    Game
    | Art
    | Music
    | Programming
    deriving (Generic, Show, Eq)

instance ToJSON Category where

type Award = (Int, Category)

data GameInfo = GameInfo
    { id :: Maybe Int
    , year :: Int
    , title :: T.Text
    , description :: T.Text
    , awards :: [Award]
    }
    deriving (Generic, Show, Eq)

instance ToJSON GameInfo where

data GamesFilter =
    NoFilter
    | ByYear Int

toCategory :: Int -> Category
toCategory 1 = Game
toCategory 2 = Art
toCategory 3 = Music
toCategory 4 = Programming
toCategory wrongCategory = error $ "Non-existing category: " ++ show wrongCategory
