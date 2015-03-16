module Handler.MergeSchools (getMergeSchoolsR, postMergeSchoolsR) where

import Import
import Control.Monad
import Common
import Prelude

data FormData = FormData
    { formMerged :: [Entity School]
    , formName :: Text
    }

getMergeSchoolsR :: Text -> Handler Html
getMergeSchoolsR cityname = do
    Entity cityId _ <- runDB $ getBy404 $ UniqueCity cityname
    (formWidget, formEnctype) <- generateFormPost $ schoolsForm cityId
    defaultLayout $ do
        setTitleI MsgMergeSchoolsTitle
        $(widgetFile "merge-schools")

postMergeSchoolsR :: Text -> Handler Html
postMergeSchoolsR cityname = do
    Entity cityId _ <- runDB $ getBy404 $ UniqueCity cityname
    ((formResults, _), _) <- runFormPost $ schoolsForm cityId
    case formResults of
        FormSuccess request -> do
            let ids = map entityKey (formMerged request)
            let target = head ids
            runDB $ do
                serializableTransaction
                forM_ (tail ids) $ \schoolId -> do
                    updateWhere [ParticipationSchool ==. Just schoolId] [ParticipationSchool =. Just target]
                    delete schoolId
                update target [SchoolName =. formName request]
                regenerateRanking
            setMessageI MsgMergeSchoolsDone
            redirect $ MergeSchoolsR cityname
        FormFailure reasons -> invalidArgs reasons
        _ -> redirect $ MergeSchoolsR cityname
                

schoolsForm :: CityId -> Form FormData
schoolsForm cityId = renderBootstrap3 layout (schoolsAForm cityId)
    where
        layout = BootstrapHorizontalForm (ColSm 0) (ColSm 4) (ColSm 0) (ColSm 6)

schoolsAForm :: CityId -> AForm Handler FormData
schoolsAForm cityId = FormData
    <$> areq (checkboxesField values) (fieldSettingsLabel MsgMergeSchoolsSelectSchools) Nothing
    <*> areq textField (fieldSettingsLabel MsgMergeSchoolsName) Nothing
    <* bootstrapSubmit (BootstrapSubmit MsgMergeSchoolsSubmit "btn-warning" [])
    where
        values = optionsPersist [SchoolCity ==. cityId] [Asc SchoolName] schoolName
 
