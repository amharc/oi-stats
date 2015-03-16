module Handler.Lang (postLangR) where

import Import

postLangR :: Handler Html
postLangR = do
    (lang, dest) <- runInputPost $ (,) <$> ireq textField "lang" <*> iopt textField "dest"
    setLanguage lang
    case dest of
        Just url -> redirect url
        Nothing -> redirect HomeR
