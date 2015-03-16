module Handler.School (getSchoolR) where

import Import
import Common
import Control.Arrow
import qualified Database.Esqueleto as E

getSchoolR :: SchoolId -> Handler Html
getSchoolR schoolId = do
    (school, city, rows') <- runDB $ do
        school' <- get404 schoolId
        city <- get404 (schoolCity school')
        rows <- E.select $
            E.from $ \(contestant `E.InnerJoin` participation `E.InnerJoin` stage `E.InnerJoin` olympiad, school) -> do
                E.on (olympiad E.^. OlympiadId E.==. stage E.^. StageOlympiad)
                E.on (stage E.^. StageId E.==. participation E.^. ParticipationStage)
                E.on (participation E.^. ParticipationContestant E.==. contestant E.^. ContestantId)
                E.where_ (participation E.^. ParticipationSchool E.==. E.just (E.val schoolId) E.&&. school E.?. SchoolId E.==. E.just (E.val schoolId))
                E.orderBy [E.asc (contestant E.^. ContestantSurname),
                           E.asc (contestant E.^. ContestantName),
                           E.asc (contestant E.^. ContestantId), -- in case of same name&surname
                           E.asc (olympiad E.^. OlympiadIdent),
                           E.asc (stage E.^. StageStage)]
                return (contestant, (olympiad, (participation, stage, school)))
        return (school', city, rows)
    let rows = map (second groupByFirst) $ groupByFirst rows'
    
    defaultLayout $ do
        setTitleI $ MsgSchoolTitle (schoolName school) (cityName city)
        $(widgetFile "school")
