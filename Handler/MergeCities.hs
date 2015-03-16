module Handler.MergeCities (getMergeCitiesR, postMergeCitiesR) where

import Control.Monad
import Common
import Data.Maybe
import Import

data FormData = FormData
    { formName :: Text
    , formLatitude :: Maybe Double
    , formLongitude :: Maybe Double
    , formMerged :: Maybe [Entity City]
    }

getMergeCitiesR :: Text -> Handler Html
getMergeCitiesR cityname = do
    ent <- runDB $ getBy404 $ UniqueCity cityname
    (formWidget, formEnctype) <- generateFormPost $ citiesForm ent
    defaultLayout $ do
        setTitleI MsgMergeCitiesTitle
        $(widgetFile "merge-cities")

postMergeCitiesR :: Text -> Handler Html
postMergeCitiesR cityname = do
    ent@(Entity cityId _) <- runDB $ getBy404 $ UniqueCity cityname
    ((formResults, _), _) <- runFormPost $ citiesForm ent
    case formResults of
        FormSuccess request -> do
            let ids = map entityKey $ fromMaybe [] $ formMerged request
            runDB $ do
                serializableTransaction
                forM_ ids $ \other -> do
                    selectList [SchoolCity ==. other] [] >>= mapM_ (fixSchool cityId)
                    selectList [ContestantCity ==. other] [] >>= mapM_ (fixContestant cityId)
                    delete other
                replace cityId $ City (formName request) (formLatitude request) (formLongitude request)
                regenerateRanking
            setMessageI MsgMergeCitiesDone
            redirect $ CityR $ formName request
        FormFailure reasons -> invalidArgs reasons
        _ -> redirect $ MergeCitiesR cityname
    where
        fixGeneric new oldId fun field = do
            ret <- checkUnique new
            case ret of
                Nothing -> replace oldId new
                Just unique -> do
                    Just (Entity otherId _) <- getBy unique
                    updateWhere [field ==. fun oldId] [field =. fun otherId]
                    delete oldId

        fixContestant cityId (Entity contestantId contestant) = fixGeneric
            (contestant {contestantCity = cityId})
            contestantId
            id
            ParticipationContestant

        fixSchool cityId (Entity schoolId school) = fixGeneric
            (school {schoolCity = cityId})
            schoolId
            Just
            ParticipationSchool

citiesForm :: Entity City -> Form FormData
citiesForm (Entity cityId city) = renderBootstrap3 layout $ FormData
    <$> areq textField (fieldSettingsLabel MsgMergeCitiesName) (Just $ cityName city)
    <*> aopt doubleField (fieldSettingsLabel MsgMergeCitiesLatitude) (Just $ cityLatitude city)
    <*> aopt doubleField (fieldSettingsLabel MsgMergeCitiesLongitude) (Just $ cityLongitude city)
    <*> aopt (checkboxesField values) (fieldSettingsLabel $ MsgMergeCitiesSelectCities $ cityName city) Nothing
    <* bootstrapSubmit (BootstrapSubmit MsgMergeCitiesSubmit "btn-warning" [])
    where
        values = optionsPersist [CityId !=. cityId] [Asc CityName] cityName
        layout = BootstrapHorizontalForm (ColSm 0) (ColSm 4) (ColSm 0) (ColSm 6)
