module Handler.StageChart (getStageChartR) where

import Import
import Common
import Control.Arrow
import Control.Monad
import Data.Function
import Data.Maybe
import qualified Data.List as L
import qualified Database.Esqueleto as E
import qualified Data.Text as T

getStageChartR :: Int -> Int -> Handler Html
getStageChartR olympiadNum stageNum = do
    (stage, olympiad, citiesAwardCnt, citiesAwardTotal) <- runDB $ do
        Entity olympiadId olympiad <- getBy404 $ UniqueOlympiad olympiadNum
        Entity stageId stage <- getBy404 $ UniqueStage olympiadId stageNum

        let joinCommon participation contestant city = do
            E.on (city E.^. CityId E.==. contestant E.^. ContestantCity)
            E.on (contestant E.^. ContestantId E.==. participation E.^. ParticipationContestant)
            E.where_ (participation E.^. ParticipationStage E.==. E.val stageId)

        citiesAwardCnt <- E.select $
            E.from $ \(participation `E.InnerJoin` contestant `E.InnerJoin` city) -> do
                joinCommon participation contestant city
                E.groupBy (city E.^. CityId, city E.^. CityName, participation E.^. ParticipationAward)
                E.orderBy [E.asc (city E.^. CityName), E.asc (city E.^. CityId), E.asc (participation E.^. ParticipationAward)]
                let cnt = E.countRows :: E.SqlExpr (E.Value Int)
                return (city E.^. CityName, (participation E.^. ParticipationAward, cnt))

        citiesAwardTotal <- E.select $
            E.from $ \(participation `E.InnerJoin` contestant `E.InnerJoin` city) -> do
                joinCommon participation contestant city
                E.groupBy (city E.^. CityId, city E.^. CityLatitude, city E.^. CityLongitude, city E.^. CityName)
                let cnt = E.countRows :: E.SqlExpr (E.Value Int)
                return (city, cnt)

        return ( stage
               , olympiad
               , processCitiesAwards (stageFinal stage) $ map (E.unValue *** E.unValue *** E.unValue) citiesAwardCnt
               , mapMaybe processCityAwardTotal citiesAwardTotal
               )

    defaultLayout $ do
        setTitleI $ MsgStageTitle (olympiadIdent olympiad) (stageStage stage)
        addScriptRemote "https://www.google.com/jsapi"
        addScript $ StaticR js_jquery_tablesorter_min_js
        $(widgetFile "stage-chart")
    where
        processCitiesAwards False citiesCnt = map (\(city, (_, cnt)) -> [city, T.pack $ show cnt]) citiesCnt
        processCitiesAwards True citiesCnt = map process grouped
            where
                grouped = map (\l -> (fst $ L.head l, map snd l)) $ L.groupBy ((==) `on` fst) citiesCnt
                process (city, awards) = 
                    let res award = fromMaybe 0 $ L.lookup award awards in
                    let cnts = map (res . Just) $ enumFromTo LaureateI Finalist in
                    city : map (T.pack . show) (sum cnts : cnts)
        processCityAwardTotal (entityVal -> city, toJSON . E.unValue -> cnt) = do
            lat <- liftM toJSON $ cityLatitude city
            lng <- liftM toJSON $ cityLongitude city
            return [lat, lng, toJSON $ cityName city, cnt]
            
