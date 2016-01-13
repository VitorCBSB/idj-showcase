{-# LANGUAGE OverloadedStrings #-}

module Query where

import qualified Data.Text as T
import Data.Maybe (fromJust)
import Snap
import Snap.Snaplet.Auth
import Snap.Snaplet.SqliteSimple

import Types

getUserRealName :: (HasSqlite m, MonadSnap m) => UserId -> m T.Text
getUserRealName uid =
    do  allResults <- query "SELECT real_name FROM custom_user WHERE user_id = ?" (Only $ unUid uid)
        let result = Prelude.head . Prelude.head $ allResults -- This shouldn't fail, so it's okay.
        return result

getGamesFromDB :: (HasSqlite m, MonadSnap m) => GamesFilter -> m [GameInfo]
getGamesFromDB filter =
    do  gamesList <- case filter of
            NoFilter -> query_ "SELECT id, year, title, description FROM game WHERE approved = 1"
            ByYear year -> query "SELECT id, year, title, description FROM game WHERE approved = 1 AND year = ?" (Only year)
        mapM (\(id, y, t, d) -> do  awards <- getGameAwards (fromJust id)
                                    return $ GameInfo id y t d awards) gamesList

getGameAwards :: (HasSqlite m, MonadSnap m) => Int -> m [Award]
getGameAwards gameId =
    do  awardsList <- query "SELECT place, category_id FROM game_awards WHERE game_id = ?" (Only gameId)
        return $ map (\(place, catId) -> (place, toCategory catId)) awardsList

addExtraUserInfo :: (HasSqlite m, MonadSnap m) => UserId -> T.Text -> m ()
addExtraUserInfo uid realName =
    do  execute "INSERT INTO custom_user (user_id, real_name) VALUES (?, ?)" (unUid uid, realName)
        return ()

deleteExtraUserInfo :: (HasSqlite m, MonadSnap m) => UserId -> m ()
deleteExtraUserInfo uid =
    do  execute "DELETE FROM custom_user WHERE user_id = ?" (Only $ unUid uid)
        return ()

getPendingReviews :: (HasSqlite m, MonadSnap m) => m Int
getPendingReviews =
    do  allResults <- query_ "SELECT count(*) from game where approved = 0"
        let result = Prelude.head . Prelude.head $ allResults
        return result
