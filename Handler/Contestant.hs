module Handler.Contestant (getContestantR) where

import Import
import Common
import Control.Monad
import qualified Database.Esqueleto as E

getContestantR :: ContestantId -> Handler Html
getContestantR contestantId = do
    (contestant, city, participations) <- runDB $ do
        contestant <- get404 contestantId
        city <- get404 (contestantCity contestant)
        participations <- liftM groupByFirst $ E.select $
            E.from $ \(participation `E.InnerJoin` stage `E.InnerJoin` olympiad `E.LeftOuterJoin` school) -> do
                E.on (school E.?. SchoolId E.==. participation E.^. ParticipationSchool)
                E.on (olympiad E.^. OlympiadId E.==. stage E.^. StageOlympiad)
                E.on (stage E.^. StageId E.==. participation E.^. ParticipationStage)
                E.where_ (participation E.^. ParticipationContestant E.==. E.val contestantId)
                E.orderBy [E.desc (olympiad E.^. OlympiadIdent),
                           E.desc (stage E.^. StageStage)]
                return (olympiad, (participation, stage, school))
        return (contestant, city, participations)

    defaultLayout $ do
        setTitleI $ MsgContestantTitle (contestantName contestant) (contestantSurname contestant)
        $(widgetFile "contestant")
