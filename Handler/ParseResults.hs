module Handler.ParseResults (getParseResultsR, postParseResultsR) where

import Import
import Common
import Data.Maybe
import Control.Monad
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Data.ByteString.Char8 as B
import Text.Read
import Network.URI
import Network.HTTP
import Text.XML.HXT.Core

data OlympiadFormData = OlympiadFormData
    { formOlympiad :: Int
    , formStage :: Int
    , formHTML :: Maybe Text
    }

data OlympiadResultsData = OlympiadResultsData
    { resultsNameIsSeparate :: Bool
    , resultsClassIsPresent :: Bool
    , resultsSchoolIsPresent :: Bool
    }

getParseResultsR :: Handler Html
getParseResultsR = do
    (formWidget, formEnctype) <- generateFormPost urlForm
    defaultLayout $ do
        setTitleI MsgParseResultsTitle
        $(widgetFile "parse-results-request")

processResponse :: OlympiadFormData -> String -> HandlerT App IO ()
processResponse form body = runDB $ do
    serializableTransaction 
    olympiad <- insertOrGet $ Olympiad $ formOlympiad form
    stage <- insert $ Stage olympiad (formStage form)

    (headers : _) <- liftIO $ runX (parseHTML body >>> selectHeaders)
    results' <- liftIO $ runX (parseHTML body >>> selectResults)

    let resultsData = OlympiadResultsData ("nazwisko" `elem` headers) ("klasa" `elem` headers) (any (T.isInfixOf "szkoł") headers)
    let tasks = map (\t -> Task stage (T.last t == '*') t) $ L.drop (headercols resultsData) $ L.init headers

    taskids <- insertMany tasks

    let results = concat $ zipWith (map . (,)) (awards $ L.length results') results'
    mapM_ (toSubmissions stage taskids resultsData) results
    regenerateRanking
    where
        insertOrGet = liftM (either entityKey id) . insertBy

        awards 1 = L.repeat Nothing
        awards 4 =  map Just [LaureateI, LaureateII, LaureateIII, Finalist]
        awards 5 = map Just $ enumFrom LaureateI
        awards x = error $ "Wrong number of awards: " ++ show x

        headercols resultsData = 3
                + iverson (resultsNameIsSeparate resultsData)
                + iverson (resultsClassIsPresent resultsData)
                + iverson (resultsSchoolIsPresent resultsData)
            where
                iverson True = 1
                iverson False = 0
            
        parseHTML = readString [ withValidate no
                               , withParseHTML yes
                               , withWarnings no
                               ]

        selectResults = multi (hasName "table") >>> 
            hasAttrValue "class" (== "results_table") >>> 
            listA (deep (isElem >>> hasName "tr") >>> 
                listA (deepest (hasNameWith ((`elem` ["td", "a"]) . localPart)) //> getText >>> arr prepareString))

        selectHeaders = multi (hasName "table") >>> 
            hasAttrValue "class" (== "results_table") >>> 
            listA (deep (isElem >>> hasName "th") /> getText >>>
                arr prepareString)

        prepareString = T.strip . E.decodeUtf8 . B.pack

        parsePart1 r@(resultsNameIsSeparate -> True) (_ : name : surname : rest) = Just (name, surname, parsePart2 r rest)
        parsePart1 r (_ : (T.words -> wr) : rest) = Just (T.unwords $ L.init wr, L.last wr, parsePart2 r rest)
        parsePart1 _ _ = Nothing

        parsePart2 r@(resultsClassIsPresent -> True) (cls : rest) = (Just cls, parsePart3 r rest)
        parsePart2 r@_ rest = (Nothing, parsePart3 r rest)

        parsePart3 (resultsSchoolIsPresent -> True) (schoolname : rest) = (Just schoolname, rest)
        parsePart3 _ rest = (Nothing, rest)

        toSubmissions stage tasks resultsData (award, parsePart1 resultsData -> Just (name, surname, (cls, (schoolname, cityname : results)))) = do
            city <- getCity cityname
            school <- case schoolname of
                Nothing -> return Nothing
                Just val -> liftM Just $ insertOrGet $ School val city
            contestant <- insertOrGet $ Contestant name surname city
            let pointsSum = if L.null results then Nothing else readMaybe $ T.unpack $ L.last results
            participation <- insert $ Participation contestant stage cls award pointsSum school

            let newresults = L.drop (L.length results - L.length tasks - 1) results
            -- repeat "" is necessary because some (but not all) result pages omit all results of finalists
            zipWithM_ (toSubmission participation) tasks (map T.unpack newresults ++ repeat "")
            where
                toSubmission participation task result = insert_ $ Submission task participation (readMaybe result)
        toSubmissions _ _ _ _ = return ()

        getCity cityname = do
            mcity <- getBy $ UniqueCity cityname
            liftM entityKey $ case mcity of
                Just city -> return city
                Nothing ->  prepareCity cityname >>= flip upsert []

        prepareCity cityname = do
            let uriString = "http://maps.googleapis.com/maps/api/geocode/xml?language=pl&region=pl&address=\"" ++ betterCityName ++ "\""
            let uri = fromJust $ parseURI $ escapeURIString isAllowedInURI uriString
            eresp <- liftIO $ simpleHTTP $ Request uri GET [] ""
            case eresp of
                Left _ ->
                    return $ City cityname Nothing Nothing
                Right (rspBody -> llbody) -> do
                    list <- liftIO $ runX $ readString [] llbody >>>
                        (multi (hasName "location") >>>
                            (deep (hasName "lat") /> getText &&&
                             deep (hasName "lng") /> getText))
                        &&&
                        (deep (hasName "address_component") >>>
                            (deep (hasName "type") >>> getChildren >>> hasText (== "locality"))
                            &&&
                            (deep (hasName "long_name") /> getText >>> arr prepareString))
                    case list of
                        ((lat, lng), (_, name)) : _ -> return $ City name (readMaybe lat) (readMaybe lng)
                        _ -> return $ City cityname Nothing Nothing
            where
                betterCityName
                    | T.any (== ',') cityname = T.unpack cityname
                    | otherwise = T.unpack cityname ++ ", Polska"
                    

processURI :: OlympiadFormData -> URI -> HandlerT App IO ()
processURI form uri = do    
    eresp <- liftIO $ simpleHTTP $ Request uri GET [] ""
    case eresp of
        Left _ ->
            error "HTTP error"
        Right res | rspCode res == (2, 0, 0) ->
            processResponse form $ rspBody res
        Right (rspCode -> (a, b, c)) ->
            error $ "Wrong server response code" ++ show a ++ show b ++ show c

postParseResultsR :: Handler Html
postParseResultsR = do
    ((result, _), _) <- runFormPost urlForm
    case result of
        FormSuccess res -> do
            case formHTML res of
                Nothing -> do
                    let uriString = "http://oi.edu.pl/l/" ++ show (formOlympiad res) ++ "oi_" ++ show (formStage res) ++ "etap_wyniki/"
                    case parseURI uriString of
                        Nothing ->
                            invalidArgs [T.pack uriString]
                        Just uri ->
                            processURI res uri
                Just body -> processResponse res $ B.unpack $ E.encodeUtf8 body

            setMessageI MsgParseResultsAddedData
            redirect $ StageR (formOlympiad res) (formStage res)

        FormFailure reasons -> invalidArgs reasons
        _ -> redirect ParseResultsR

urlForm :: Form OlympiadFormData
urlForm = renderBootstrap3 layout $ OlympiadFormData
    <$> areq (checkBool (>0) MsgParseResultsNegativeOlympiad intField) (fieldSettingsLabel MsgParseResultsOlympiadIdent) Nothing
    <*> areq (selectFieldList stages) (fieldSettingsLabel MsgParseResultsStageNumber) Nothing
    <*> aopt textHtmlField (fieldSettingsLabel MsgParseResultsHTML) Nothing
    <*  bootstrapSubmit (BootstrapSubmit MsgParseResultsParse "btn-primary" [])
    where
        layout = BootstrapHorizontalForm (ColSm 0) (ColSm 4) (ColSm 0) (ColSm 6)
        stages :: [(Text, Int)]
        stages = map ((,) =<< T.pack . show) [1, 2, 3]

        textHtmlField :: Monad m => RenderMessage (HandlerSite m) FormMessage => Field m Text
        textHtmlField = Field
            { fieldParse = parseHelper Right
            , fieldView = \theId name attrs val isReq ->
                [whamlet|
                    $newline never
                    <textarea id="#{theId}" name="#{name}" *{attrs} type="text" :isReq:required>#{either id id val}
                |]
            , fieldEnctype = UrlEncoded
            }
