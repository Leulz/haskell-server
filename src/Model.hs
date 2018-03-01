{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module Model where

import ClassyPrelude.Yesod
import Database.Persist.Quasi

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")

instance ToJSON (Entity User) where
    toJSON (Entity _ u)  = object
        [ "googleIdentifier" .= userGoogleIdentifier u
        , "nome" .= userNome u
        , "email" .= userEmail u
        , "matricula" .= userMatricula u
        ]

instance FromJSON User where
    parseJSON (Object o) = User
        <$> o .: "googleIdentifier"
        <*> o .: "nome"
        <*> o .: "email"
        <*> o .: "matricula"
    parseJSON _ = mzero