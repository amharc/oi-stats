module Handler.Olympiads (getOlympiadsR) where

import Import

getOlympiadsR :: Handler TypedContent
getOlympiadsR = do
    olympiads <- runDB $ selectList [] [Asc OlympiadIdent]
    defaultLayoutJson (do
        setTitleI MsgOlympiads
        $(widgetFile "olympiads")
        ) $ return $ object
        [ "olympiads" .= array ( flip map olympiads $ \(Entity _ olympiad) ->
            object
                [ "id" .= olympiadIdent olympiad ])
        ]
