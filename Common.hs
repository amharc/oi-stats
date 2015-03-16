module Common
    ( groupByFirst
    , innerOlympiad
    , Marker(..)
    , cityToMarker
    , centerMarker
    , stageFinal
    , regenerateRanking
    , serializableTransaction
    ) where

import Import
import Data.Function
import Control.Monad
import qualified Data.List as L
import Database.Persist.Sql
import Control.Monad.Trans.Reader
import Prelude

groupByFirst :: Eq (Key record) => [(Entity record, b)] -> [(Entity record, [b])]
groupByFirst = map (\l -> (fst $ L.head l, map snd l)) . L.groupBy ((==) `on` (entityKey . fst))

innerOlympiad :: OlympiadId -> Olympiad -> [(Entity Participation, Entity Stage, Maybe (Entity School))] -> WidgetT App IO ()
innerOlympiad _ olympiad list = $(widgetFile "inner-olympiad")

data Marker = Marker Double Double

instance ToJSON Marker where
    toJSON (Marker lat lng) = object [ "lat" .= lat, "lng" .= lng ]

cityToMarker :: City -> Maybe Marker
cityToMarker = (liftM2 . liftM2) Marker cityLatitude cityLongitude

centerMarker :: Marker
centerMarker = Marker 52 21

stageFinal :: Stage -> Bool
stageFinal stage = stageStage stage == 3

regenerateRanking :: forall (m :: * -> *). MonadIO m => ReaderT SqlBackend m ()
regenerateRanking = do
    (_ :: [Single (Maybe Text)]) <- rawSql "SELECT generate_ranking()" []
    return ()

serializableTransaction :: forall (m :: * -> *). MonadIO m => ReaderT SqlBackend m ()
serializableTransaction = rawExecute "SET TRANSACTION ISOLATION LEVEL SERIALIZABLE" []
