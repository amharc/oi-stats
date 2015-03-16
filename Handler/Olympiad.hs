module Handler.Olympiad (getOlympiadR, deleteOlympiadR, postOlympiadDeleteR) where

import Import
import Control.Monad
import Common

getOlympiadR :: Int -> Handler TypedContent
getOlympiadR ident = do
    Entity olympiadId olympiad <- runDB $ getBy404 $ UniqueOlympiad ident
    stages <- runDB $ selectList [StageOlympiad ==. olympiadId] [Asc StageStage]
    mu <- maybeAuth
    let viewerIsAdmin = maybe False (userIsAdmin . entityVal) mu
    let json = object ["id" .= ident, ("stages", array $ map (stageStage . entityVal) stages)]
    serverForbidsREST <- fmap extraServerForbidsREST getExtra
    let deleter = if serverForbidsREST then (OlympiadDeleteR, "POST" :: Text) else (OlympiadR, "DELETE")
    flip defaultLayoutJson (return json) $ do
        setTitleI $ MsgOlympiadName $ olympiadIdent olympiad
        $(widgetFile "olympiad")

deleteOlympiadR :: Int -> Handler ()
deleteOlympiadR ident = do
    runDB $ liftM entityKey (getBy404 $ UniqueOlympiad ident) >>= deleteCascade >> regenerateRanking
    setMessageI MsgOlympiadDeleted

postOlympiadDeleteR :: Int -> Handler ()
postOlympiadDeleteR = deleteOlympiadR
