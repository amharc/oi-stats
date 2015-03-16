module Handler.Stage (getStageR, deleteStageR, postStageDeleteR) where

import Import
import Common
import Control.Arrow
import Data.Maybe
import Data.Function
import qualified Data.List as L
import qualified Database.Esqueleto as E
import qualified Data.Text as T

getStageR :: Int -> Int -> Handler Html
getStageR olympiadNum stageNum = do
    (stage, olympiad, tasks, ranking) <- runDB $ do
        Entity olympiadId olympiad <- getBy404 $ UniqueOlympiad olympiadNum
        Entity stageId stage <- getBy404 $ UniqueStage olympiadId stageNum
        tasks <- selectList [TaskStage ==. stageId] [Desc TaskSample, Asc TaskName]
        ranking <- E.select $
            E.from $ \ranking -> do
                E.where_ (ranking E.^. RankingStage E.==. E.val stageId)
                E.orderBy [ E.desc $ E.coalesce [ranking E.^. RankingSum, E.just $ E.val (-1)]
                          , E.asc  $ ranking E.^. RankingSurname
                          , E.asc  $ ranking E.^. RankingName
                          , E.asc  $ ranking E.^. RankingContestantid
                          , E.desc $ ranking E.^. RankingTasksample
                          , E.asc  $ ranking E.^. RankingTaskname
                          , E.asc  $ ranking E.^. RankingTask
                          ]
                return ranking
        return (stage, olympiad, tasks, ranking)

    let nrows =  L.groupBy ((==) `on` rankingContestantid . entityVal) ranking
    let markersWithNames = mapMaybe (maybeMarker . entityVal . L.head) nrows

    mu <- maybeAuth
    let viewerIsAdmin = maybe False (userIsAdmin . entityVal) mu

    apiKey <- fmap extraAPIKey getExtra
    serverForbidsREST <- fmap extraServerForbidsREST getExtra
    let deleter = if serverForbidsREST then (StageDeleteR, "POST" :: Text) else (StageR, "DELETE")
    defaultLayout $ do
        setTitleI $ MsgStageTitle (olympiadIdent olympiad) (stageStage stage)
        addScriptRemote $ T.concat ["https://maps.googleapis.com/maps/api/js?libraries=visualization&v=3.exp&key=", apiKey]
        addScriptRemote "https://code.jquery.com/jquery-1.11.2.min.js"
        addScriptRemote "https://www.google.com/jsapi"
        $(widgetFile "stage")
    
    where
        maybeMarker (rankingCity &&& rankingLatitude &&& rankingLongitude -> (name, (Just lat, Just lng))) = Just (name, Marker lat lng)
        maybeMarker _ = Nothing

deleteStageR :: Int -> Int -> Handler ()
deleteStageR olympiadNum stageNum = do
    runDB $ do
        Entity olympiadId _ <- getBy404 $ UniqueOlympiad olympiadNum
        Entity stageId _ <- getBy404 $ UniqueStage olympiadId stageNum
        deleteCascade stageId
        regenerateRanking
    setMessageI MsgStageDeleted

postStageDeleteR :: Int -> Int -> Handler ()
postStageDeleteR = deleteStageR
