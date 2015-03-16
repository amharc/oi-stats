module Handler.City (getCityR) where

import Import
import Common
import Control.Arrow
import Data.Maybe
import qualified Database.Esqueleto as E
import qualified Data.Text as T

getCityR :: Text -> Handler Html
getCityR name = do
    (city, schools, rows') <- runDB $ do
        Entity cityId city <- getBy404 $ UniqueCity name
        schools <- selectList [SchoolCity ==. cityId] [Asc SchoolName]
        rows <- E.select $
            E.from $ \(contestant `E.InnerJoin` participation `E.InnerJoin` stage `E.LeftOuterJoin` school `E.InnerJoin` olympiad) -> do
                E.on (olympiad E.^. OlympiadId E.==. stage E.^. StageOlympiad)
                E.on (participation E.^. ParticipationSchool E.==. school E.?. SchoolId)
                E.on (stage E.^. StageId E.==. participation E.^. ParticipationStage)
                E.on (participation E.^. ParticipationContestant E.==. contestant E.^. ContestantId)
                E.where_ (contestant E.^. ContestantCity E.==. E.val cityId)
                E.orderBy [E.asc (contestant E.^. ContestantSurname),
                           E.asc (contestant E.^. ContestantName),
                           E.asc (contestant E.^. ContestantId), -- in case of same name&surname
                           E.asc (olympiad E.^. OlympiadIdent),
                           E.asc (stage E.^. StageStage)]
                return (contestant, (olympiad, (participation, stage, school)))
        return (city, schools, rows)
    let rows = map (second groupByFirst) $ groupByFirst rows'

    apiKey <- fmap extraAPIKey getExtra

    mu <- maybeAuth
    let viewerIsAdmin = maybe False (userIsAdmin . entityVal) mu

    defaultLayout $ do
        setTitleI $ MsgCityTitle (cityName city)
        addScriptRemote $ T.concat ["https://maps.googleapis.com/maps/api/js?key=", apiKey]
        $(widgetFile "city")
