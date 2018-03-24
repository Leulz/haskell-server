{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Home where
import Import
import Data.Aeson
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import ReadXLSX (containsMatricula)

-- Define our data that will be used for creating the form.
data FileForm = FileForm
    { fileInfo :: FileInfo
    , fileDescription :: Text
    }

-- TODO move these datatype definitions to an appropriate file.
data HomeResponseJSON = HomeResponseJSON
    { authId :: Text}

instance ToJSON HomeResponseJSON where
    toJSON (HomeResponseJSON a)  = object
        [ "authId" .= a ]

instance FromJSON HomeResponseJSON where
    parseJSON = withObject "HomeResponseJSON" $ \v -> HomeResponseJSON
        <$> v .: "authId"

--Serves HTML or JSON depending on the "Accept" header coming from the request.
getHomeR :: Handler TypedContent
getHomeR = do
    muid <- maybeAuthId
    selectRep $ do
        provideRep $ defaultLayout $ do
            $(widgetFile "auth")
        provideJson $ HomeResponseJSON $ pack $ show muid

postHomeR :: Handler Html
postHomeR = do
    defaultLayout $ do
        aDomId <- newIdent
        setTitle "Welcome To Yesod!"
        $(widgetFile "homepage")

getUsuarioR :: Text -> Handler Value
getUsuarioR googleIdent = do
    user <- runDB $ getBy404 $ UniqueUser googleIdent
    return $ object ["user" .= user]

--FIXME Returning error 500 when one tries to register a user already registered
postUsuariosR :: Handler ()
postUsuariosR = do
    user <- requireJsonBody :: Handler User
    isPermittedMatricula <- liftIO $ containsMatricula $ unpack $ userMatricula user
    case isPermittedMatricula of
        True -> do
            _    <- runDB $ insert user
            sendResponseStatus status201 ("CREATED" :: Text)
        False -> sendResponseStatus status400 ("MATRÍCULA INVÁLIDA" :: Text)
    

sampleForm :: Form FileForm
sampleForm = renderBootstrap3 BootstrapBasicForm $ FileForm
    <$> fileAFormReq "Choose a file"
    <*> areq textField textSettings Nothing
    -- Add attributes like the placeholder and CSS classes.
    where textSettings = FieldSettings
            { fsLabel = "What's on the file?"
            , fsTooltip = Nothing
            , fsId = Nothing
            , fsName = Nothing
            , fsAttrs =
                [ ("class", "form-control")
                , ("placeholder", "File description")
                ]
            }
