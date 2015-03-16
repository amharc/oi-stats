module Handler.ContestantSearch (getContestantSearchR) where

import Import
import Data.Function
import Data.Maybe
import Text.ParserCombinators.Parsec as P hiding (Parser)
import qualified Database.Esqueleto as E
import qualified Data.Text as T
import Text.Parsec.Text
import qualified Data.List as L

data ContestantFormData = ContestantFormData
    { formName :: Maybe Text
    , formSurname :: Maybe Text
    , formCity :: Maybe Text
    , formQuery :: Maybe Text
    }

type SumMerger = E.SqlExpr (E.Value (Maybe Int)) -> E.SqlExpr (E.Value (Maybe Int)) -> E.SqlExpr (E.Value Bool)

data QueryConstraint where
    StageParticipationConstraint :: Maybe Int -> Maybe Int -> QueryConstraint
    StageSumConstraint :: Maybe Int -> Maybe Int -> SumMerger -> Int -> QueryConstraint
    AndConstraint :: [QueryConstraint] -> QueryConstraint
    OrConstraint :: [QueryConstraint] -> QueryConstraint
    NotConstraint :: QueryConstraint -> QueryConstraint

makeConstraint :: E.SqlExpr (E.Value (Key Contestant)) -> QueryConstraint -> E.SqlExpr (E.Value Bool)
makeConstraint contestantId (StageSumConstraint olympiadNum stageNum fun pts) = E.exists $
    E.from $ \(participation `E.InnerJoin` stage `E.InnerJoin` olympiad) -> do
        E.on (stage E.^. StageOlympiad E.==. olympiad E.^. OlympiadId)
        E.on (participation E.^. ParticipationStage E.==. stage E.^. StageId)
        E.where_ $ L.foldl1 (E.&&.)
            $ constraint (stage E.^. StageStage) stageNum
            . constraint (olympiad E.^. OlympiadIdent) olympiadNum
            $ [ fun (participation E.^. ParticipationSum) (E.val $ Just pts)
              , participation E.^. ParticipationContestant E.==. contestantId
              ]
    where
        constraint _ Nothing = id
        constraint field (Just v) = (:) $ field E.==. E.val v

makeConstraint contestantId (StageParticipationConstraint olympiadNum stageNum) =
    makeConstraint contestantId (StageSumConstraint olympiadNum stageNum (const (const $ E.val True)) undefined)

makeConstraint contestantId (AndConstraint constraints) =
    L.foldl1 (E.&&.) (map (makeConstraint contestantId) constraints)

makeConstraint contestantId (OrConstraint constraints) =
    L.foldl1 (E.||.) (map (makeConstraint contestantId) constraints)

makeConstraint contestantId (NotConstraint constraint) =
    E.not_ (makeConstraint contestantId constraint)

parseNumber :: Parser (Maybe Int)
parseNumber =
    (Just . read) <$> many1 digit
    P.<|>
    (char '*' >> return Nothing)

skipSpaces :: Parser a -> Parser a
skipSpaces = between spaces spaces

parseSumMerger :: Parser SumMerger
parseSumMerger = choice
    [ string "==" >> return (E.==.)
    , string "!=" >> return (E.!=.)
    , char '>' >> option (E.>.) (char '=' >> return (E.>=.))
    , char '<' >> option (E.<.) (char '=' >> return (E.<=.))
    ]

parseStageSumConstraint :: Maybe Int -> Maybe Int -> Parser QueryConstraint
parseStageSumConstraint olympiadNum stageNum = do
    merger <- parseSumMerger
    spaces
    points <- read <$> many1 digit
    return $ StageSumConstraint olympiadNum stageNum merger points

parseStageConstraint :: Maybe Int -> Parser QueryConstraint
parseStageConstraint olympiadNum = do
    _ <- char '.'
    stageNum <- parseNumber
    spaces
    option (StageParticipationConstraint olympiadNum stageNum) (parseStageSumConstraint olympiadNum stageNum)

parseOlympiadOrStageConstraint :: Parser QueryConstraint
parseOlympiadOrStageConstraint = skipSpaces $ do
    olympiadNum <- parseNumber
    option (StageParticipationConstraint olympiadNum Nothing) (parseStageConstraint olympiadNum)

parseSingleConstraint :: Parser QueryConstraint
parseSingleConstraint = skipSpaces $
    between (char '(') (char ')') parseConstraint
    P.<|>
    NotConstraint <$> (string "not" >> parseSingleConstraint)
    P.<|>
    parseOlympiadOrStageConstraint

parseAndConstraint :: Parser QueryConstraint
parseAndConstraint = skipSpaces $ AndConstraint <$> sepBy1 parseSingleConstraint (string "and")

parseOrConstraint :: Parser QueryConstraint
parseOrConstraint = skipSpaces $ OrConstraint <$> sepBy1 parseAndConstraint (string "or")

parseConstraint :: Parser QueryConstraint
parseConstraint = skipSpaces parseOrConstraint

parseLine :: Parser QueryConstraint
parseLine = do
    c <- parseConstraint
    eof
    return c

makeCityMapJSON :: [(t, Entity City)] -> Maybe Value
makeCityMapJSON lst@((_, Entity _ (City name (Just lat) (Just lng))) : _) = Just $ array [toJSON lat, toJSON lng, toJSON name, toJSON $ length lst]
makeCityMapJSON _ = Nothing

getContestantSearchR :: Handler Html
getContestantSearchR = do
    ((formResults, formWidget), formEnctype) <- runFormGet contestantForm
    results <-
        case formResults of
            FormSuccess query -> do
                queryConstraints <- getConstraints (formQuery query)
                let constraints contestant city = foldl (E.&&.) (E.val True)
                        $ constraint (formName query) (contestant E.^. ContestantName)
                        . constraint (formSurname query) (contestant E.^. ContestantSurname)
                        . constraint (formCity query) (city E.^. CityName)
                        $ map (makeConstraint $ contestant E.^. ContestantId) queryConstraints
                runDB $ E.select $
                    E.from $ \(contestant `E.InnerJoin` city) -> do
                        E.on (contestant E.^. ContestantCity E.==. city E.^. CityId)
                        E.where_ (constraints contestant city)
                        E.orderBy [ E.asc (contestant E.^. ContestantSurname)
                                  , E.asc (contestant E.^. ContestantName)
                                  , E.asc (city E.^. CityName)
                                  ]
                        return (contestant, city)
            _ -> return []

    let cities = array $ mapMaybe makeCityMapJSON $ L.groupBy ((==) `on` snd) $ L.sortBy (compare `on` (cityName . entityVal . snd)) results

    defaultLayout $ do
        setTitleI MsgContestantSearchTitle
        addScriptRemote "https://www.google.com/jsapi"
        $(widgetFile "contestant-search")
    where
        constraint Nothing _ = id
        constraint (Just val) field = (:) $ field `E.like` E.val val

        getConstraints (Nothing) = return []
        getConstraints (Just query) =
            case parse parseLine "query" query of
                Left err -> invalidArgs [T.pack $ show err]
                Right c -> return [c]

maxQueryLength :: Int
maxQueryLength = 40

contestantForm :: Form ContestantFormData
contestantForm = renderBootstrap3 layout $ ContestantFormData
    <$> aopt textField (fieldSettingsLabel MsgContestantSearchName) Nothing
    <*> aopt textField (fieldSettingsLabel MsgContestantSearchSurname) Nothing
    <*> aopt textField (fieldSettingsLabel MsgContestantSearchCity) Nothing
    <*> aopt (checkBool ((< maxQueryLength) . T.length) (MsgContestantSearchTooLong maxQueryLength) textField)
        (fieldSettingsLabel MsgContestantSearchQuery) Nothing
    <*  bootstrapSubmit (BootstrapSubmit MsgContestantSearchSearch "btn-primary" [])
    where
        layout = BootstrapHorizontalForm (ColSm 0) (ColSm 4) (ColSm 0) (ColSm 6)
